{-# LANGUAGE OverloadedStrings #-}
module Graphics.Text.Character where

import           Graphics.Utils
import           Graphics.Math
import           Graphics.Types as T
import           Graphics.Text.Font
import           Graphics.Text.Shader
import           Graphics.Rendering.OpenGL hiding (Matrix, Bitmap)
import           Control.Monad.State (execState)
import           Control.Monad
import           Control.Lens
import qualified Data.IntMap as IM


loadCharMap :: TextRenderer -> String -> IO TextRenderer
loadCharMap r str = foldM loadChar r str


charIsLoaded :: TextRenderer -> Char -> Bool
charIsLoaded r c = let i = fromEnum c in
    case (IM.lookup i $ r^.atlas.atlasMap) of
                         Nothing -> False
                         Just _  -> True


loadChar :: TextRenderer -> Char -> IO TextRenderer
loadChar r c = if charIsLoaded r c then return r else loadCharacter r c


-- | Loads a character into a text renderer's atlas.
-- It does so by rendering a glyph to a texture then draws the original
-- atlas and the glyph texture into a framebuffer that is used as the new
-- atlas texture.
loadCharacter :: TextRenderer -> Char -> IO TextRenderer
loadCharacter r ' ' = return r

loadCharacter r char = do
    putStrLn $ "loadCharacter: '" ++ show char
    let fp       = r^.atlas.atlasFontFilePath
        px       = r^.atlas.atlasPxSize
        (aW, aH) = r^.atlas.atlasTextureSize
        aTex     = r^.atlas.atlasTextureObject

    -- Render the glyph into a seperate texture.
    (charTex, (FontChar (gW, gH) _ gMtrx)) <- texturizeGlyphOfEnum fp px char

    let w  = aW + gW
        h  = max aH gH
        w' = fromIntegral w
        h' = fromIntegral h
        gW' = fromIntegral gW :: GLint
        gH' = fromIntegral gH :: GLint
        gW'' = fromIntegral gW' :: GLfloat
        gH'' = fromIntegral gH' :: GLfloat


    -- Flip the charTex to be right side up.
    charTex' <- renderToTexture (gW', gH') R8 $ do
        putStrLn "Rendering glyph."
        let pj = orthoMatrix 0 gW'' gH'' 0 0 1 :: Matrix GLfloat
            mv = identityN 4 `multiply` scaleMatrix3d gW'' gH'' 1 :: Matrix GLfloat
            vs = texQuad 0 0 1 1
            us = quad 0 0 1 1

        clearColor $= Color4 0 0 0 1
        clear [ColorBuffer]
        viewport $= (Position 0 0, Size gW' gH')
        currentProgram $= (Just $ r^.textProgram.tShader.program)
        texture Texture2D $= Enabled
        activeTexture $= TextureUnit 0
        textureBinding Texture2D $= Just charTex
        r^.textProgram.setSampler $ Index1 0
        r^.textProgram.setTextColor $ Color4 1 0 0 1
        r^.textProgram.tShader.setProjection $ concat pj
        r^.textProgram.tShader.setModelview $ concat mv
        (i,j) <- bindAndBufferVertsUVs vs us
        r^.textProgram.renderTexQuad
        deleteObjectNames [i,j]

    -- Then get rid of the old char texture.
    deleteObjectName charTex

    tex <- renderToTexture (w',h') R8 $ do
        putStrLn "Rendering atlas."
        -- Now render business as usual.
        let pj  = orthoMatrix 0 (fromIntegral w) (fromIntegral h) 0 0 1 :: Matrix GLfloat
            aW' = fromIntegral aW
            aH' = fromIntegral aH

        clearColor $= Color4 0 0 0 1
        clear [ColorBuffer, DepthBuffer]
        viewport $= (Position 0 0, Size w' h')
        currentProgram $= (Just $ r^.textProgram.tShader.program)
        r^.textProgram.tShader.setProjection $ concat pj
        -- We have to render the atlas upside down.
        renderTex r aTex (0,0) (aW', aH')
        renderTex r charTex' (aW', 0) (gW'', gH'')

    deleteObjectName aTex
    deleteObjectName charTex'

    -- Update our atlas.
    return $ flip execState r $ do
        atlas.atlasMap %= IM.insert (fromEnum char) (FontChar (gW,gH) (aW,0) gMtrx)
        atlas.atlasTextureObject .= tex
        atlas.atlasTextureSize .= (w,h)


-- | Draw one character at a given pen position using the given renderer.
-- Returns the next pen position.
drawCharacter :: TextRenderer -> PenPosition -> Char -> IO PenPosition
drawCharacter r (x,y) ' ' =
    let mChar = IM.lookup 0 $ r^.atlas.atlasMap
    in
    case mChar of
        -- Worst case scenario we advance by the pixel size.
        Nothing -> return (x + fromIntegral (r^.atlas.atlasPxSize), y)
        Just c -> return $ advancePenPosition (x,y) c

drawCharacter r (x,y) char =
    let mChar = IM.lookup (fromEnum char) $ r^.atlas.atlasMap
    in
    case mChar of
        Nothing -> return (x,y)
        Just fc@(FontChar (w,h) _ (NormGMetrics (bXp, bYp) _)) -> do
            let Atlas _ tex (tSw,tSh) pxS _ = r^.atlas

            -- Find the scaled (normalized) glyph metrics and use those to
            -- typeset our character.
            -- TODO: Add kerning.
            let sW = fromIntegral w
                sH = fromIntegral h
                x' = x + sW * realToFrac bXp
                y' = (y + fromIntegral pxS) - sH * realToFrac bYp
                txy = translationMatrix3d x' y' 0
                sxy = scaleMatrix3d sW sH 1 :: Matrix GLfloat
                mv  = identityN 4 :: Matrix GLfloat
                mv' = mv `multiply` txy `multiply` sxy
                vts = quad 0 0 1 1
                uvs = fontCharUVs fc (fromIntegral tSw, fromIntegral tSh)

            -- Make our geometry vbos.
            (i,j) <- bindAndBufferVertsUVs vts uvs

            -- Do some standard GL texture stuffs and render our quad with
            -- the char's texture.
            texture Texture2D $= Enabled
            activeTexture $= TextureUnit 0
            textureBinding Texture2D $= Just tex
            r^.textProgram.setSampler $ Index1 0
            r^.textProgram.tShader.setModelview $ concat mv'

            drawArrays Triangles 0 6
            bindBuffer ArrayBuffer $= Nothing

            deleteObjectNames [i,j]

            return $ advancePenPosition (x,y) fc


-- | Returns the vertex coords of the given character at a pen position
-- and the next pen position.
-- The vertices are in pixel coordinates.
fontCharVs :: FontChar -> PenPosition -> ([GLfloat], PenPosition)
fontCharVs fc@(FontChar (w,h) _ _) (x,y) =
    (quad x y (fromIntegral w) (fromIntegral h), advancePenPosition (x,y) fc)


-- | Returns the uv coords of the given character with regard to a texture
-- size. The texture atlas has the glyphs flipped in Y.
fontCharUVs :: FontChar -> (GLfloat, GLfloat) -> [GLfloat]
fontCharUVs (FontChar (w,h) (x,y) _) (tW,tH) =
    let x' = fromIntegral x/tW
        y' = fromIntegral y/tH
        w' = fromIntegral w/tW
        h' = fromIntegral h/tH
    in quad x' y' w' h'


advancePenPosition :: PenPosition -> FontChar -> PenPosition
advancePenPosition (x,y) (FontChar (w,_) _ (NormGMetrics _ advp)) =
    let w'  = fromIntegral w
        adv = realToFrac advp * w'
    in (x + adv, y)


-- | Renders a texture object at a pen position using the program in the
-- given text renderer.
renderTex :: TextRenderer -> TextureObject -> PenPosition -> (GLfloat, GLfloat) -> IO ()
renderTex r t (x,y) (w,h) = do
    let scl  = scaleMatrix3d w h 1 :: Matrix GLfloat
        tns = translationMatrix3d x y 0 :: Matrix GLfloat
        mv  = identityN 4 `multiply` tns `multiply` scl
        vts = texQuad 0 0 1 1
        uvs = texQuad 0 0 1 1
    r^.textProgram.tShader.setModelview $ concat mv
    r^.textProgram.setSampler $ Index1 0
    r^.textProgram.setTextColor $ Color4 1 0 0 1
    texture Texture2D $= Enabled
    activeTexture $= TextureUnit 0
    textureBinding Texture2D $= Just t
    (i,j) <- bindAndBufferVertsUVs vts uvs
    drawArrays Triangles 0 6
    deleteObjectNames [i,j]
    bindBuffer ArrayBuffer $= Nothing


