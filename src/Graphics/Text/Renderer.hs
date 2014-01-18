{-# LANGUAGE OverloadedStrings #-}
module Graphics.Text.Renderer (
    module T,
    initTextRenderer,
    drawTextAt,
    loadCharMap,
) where

import           Graphics.Utils
import           Graphics.Math
import           Graphics.Types as T
import           Graphics.Text.Font
import           Graphics.Text.Shader
import           Graphics.Rendering.OpenGL hiding (Bitmap, Matrix)
import           Graphics.Rendering.OpenGL.Raw
import           Control.Monad.State (execState)
import           Control.Monad
import           Control.Lens
import           Foreign
import qualified Data.IntMap as IM


-- | Returns vertices for a two-tri quad.
-- Assumes (0,0) is the upper left, y incresing downward.
quad :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> [GLfloat]
quad x y w h = [x, y, x + w, y, x + w, y + h, x, y, x + w, y + h, x, y + h]


-- | Returns uvs for a two-tri quad.
-- Assums (0,0) is the lower left, y incresing upward.
texQuad :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> [GLfloat]
texQuad x y w h =
    [ x, y + h
    , x + w , y + h
    , x + w, y
    , x, y + h
    , x + w, y
    , x, y
    ]


sizeOfList :: [GLfloat] -> GLsizeiptr
sizeOfList = fromIntegral . (* sizeOf (undefined :: GLfloat)) . length


bindAndBufferVertsUVs :: [GLfloat] -> [GLfloat] -> IO (BufferObject, BufferObject)
bindAndBufferVertsUVs vts uvs = do
    [i,j] <- genObjectNames 2

    bindVBO i vertDescriptor $ AttribLocation 0
    withArray vts $ \ptr -> do
        bufferData ArrayBuffer $= (sizeOfList vts, ptr, StaticDraw)

    bindVBO j uvDescriptor $ AttribLocation 1
    withArray uvs $ \ptr -> do
        bufferData ArrayBuffer $= (sizeOfList uvs, ptr, StaticDraw)

    return (i,j)


initAtlas :: FilePath -> Int -> IO Atlas
initAtlas fp px = do
    -- Get the missing glyph as a texture and FontChar.
    (tex, fChar) <- texturizeGlyphOfEnum fp px '\NUL'

    -- Store that in our atlas.
    return Atlas { _atlasFontFilePath = fp
                 , _atlasTextureObject = tex
                 , _atlasTextureSize = _fcTextureSize fChar
                 , _atlasPxSize = px
                 , _atlasMap = IM.insert 0 fChar IM.empty
                 }


loadCharMap :: TextRenderer -> String -> IO TextRenderer
loadCharMap r str = foldM loadChar r str


loadChar :: TextRenderer -> Char -> IO TextRenderer
loadChar r c =
    let i = fromEnum c
    in
    case IM.lookup i (r^.atlas.atlasMap) of
        Just _  -> return r
        Nothing -> loadCharacter r c


-- | Loads a character into our atlas.
-- It does so by rendering a glyph to a texture then draws the original
-- atlas and the glyph texture into a framebuffer that is used as the new
-- atlas.
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


renderTex :: TextRenderer -> TextureObject -> (GLfloat, GLfloat) -> (GLfloat, GLfloat) -> IO ()
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


drawTextAt :: TextRenderer -> (GLfloat, GLfloat) -> String -> IO ()
drawTextAt r (x,y) = foldM_ foldCharacter (x,y)
    where foldCharacter (_,y') '\n' = return (x, y' + fromIntegral (r^.atlas.atlasPxSize))
          foldCharacter p c          = drawCharacter r p c


drawCharacter :: TextRenderer -> (GLfloat, GLfloat) -> Char -> IO (GLfloat, GLfloat)
drawCharacter r (x,y) ' ' =
    let mChar = IM.lookup 0 $ r^.atlas.atlasMap
    in
    case mChar of
        -- Worst case scenario we advance by the pixel size.
        Nothing -> return (x + fromIntegral (r^.atlas.atlasPxSize), y)
        Just (FontChar (w,_) _ (NormGMetrics _ advp)) -> do
            let w'  = fromIntegral w
                adv = realToFrac advp * w'
            return (x + adv, y)

drawCharacter r (x,y) char =
    let mChar = IM.lookup (fromEnum char) $ r^.atlas.atlasMap
    in
    case mChar of
        Nothing -> return (x,y)
        Just fc@(FontChar (w,h) _ (NormGMetrics (bXp, bYp) advp)) -> do
            let Atlas _ tex (tSw,tSh) pxS _ = r^.atlas

            -- Find the scaled (normalized) glyph metrics and use those to
            -- typeset our character.
            -- TODO: Add kerning.
            let sW = fromIntegral w
                sH = fromIntegral h
                x' = x + sW * realToFrac bXp
                y' = (y + fromIntegral pxS) - sH * realToFrac bYp
                a  = realToFrac advp * sW
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

            return (x + a, y)


-- | Returns the uv coords of the character in the font texture.
-- The texture atlas has the glyphs flipped in Y.
fontCharUVs :: FontChar -> (GLfloat, GLfloat) -> [GLfloat]
fontCharUVs (FontChar (w,h) (x,y) _) (tW,tH) =
    let x' = fromIntegral x/tW
        y' = fromIntegral y/tH
        w' = fromIntegral w/tW
        h' = fromIntegral h/tH
    in quad x' y' w' h'


initTextRenderer :: FilePath -> Int -> IO TextRenderer
initTextRenderer font px = do
    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    depthFunc $= Nothing

    putStrLn "Text shader."
    v <- makeShader VertexShader vertSrc
    f <- makeShader FragmentShader fragSrc

    p <- makeProgram [v,f] [("position", AttribLocation 0), ("uv", AttribLocation 1)]
    currentProgram $= Just p
    printError

    UniformLocation mv <- get $ uniformLocation p "modelview"
    UniformLocation pj <- get $ uniformLocation p "projection"

    let updateMV mat = withArray mat $ \ptr ->
                           glUniformMatrix4fv mv 1 1 ptr
        updatePJ mat = withArray mat $ \ptr ->
                           glUniformMatrix4fv pj 1 1 ptr

    sLoc <- get $ uniformLocation p "sampler"
    cLoc <- get $ uniformLocation p "color"
    let updateSampler s = uniform sLoc $= s
        updateColor c   = uniform cLoc $= c



    let verts = quad 0 0 1 1
        uvs   = verts
        size' = length verts * sizeOf (undefined :: Float)
    [i,j] <- genObjectNames 2

    -- Buffer the verts
    bindVBO i vertDescriptor $ AttribLocation 0
    withArray verts $ \ptr ->
        bufferData ArrayBuffer $= (fromIntegral size', ptr, StaticDraw)

    -- Buffer the uvs
    bindVBO j uvDescriptor $ AttribLocation 1
    withArray uvs $ \ptr ->
        bufferData ArrayBuffer $= (fromIntegral size', ptr, StaticDraw)


    let quadUV = do bindVBO i vertDescriptor $ AttribLocation 0
                    bindVBO j uvDescriptor $ AttribLocation 1
                    drawArrays Triangles 0 6
                    bindBuffer ArrayBuffer $= Nothing

    putStrLn $ "Initializing atlas at size " ++ show px
    a <- initAtlas font px


    return TextRenderer { _textProgram =
                            TextShaderProgram { _tShader =
                                    ShaderProgram { _program = p
                                                  , _setModelview  = updateMV
                                                  , _setProjection = updatePJ
                                                  }
                                              , _setSampler = updateSampler
                                              , _setTextColor = updateColor
                                              , _renderTexQuad = quadUV
                                              }
                        , _atlas = a
                        }

