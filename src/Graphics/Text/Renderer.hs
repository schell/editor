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
import           Control.Monad
import           Control.Lens
import           Foreign
import qualified Data.IntMap as IM


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
loadCharMap r str = do
    -- Load the font atlas for this string.
    a <- foldM (loadChar $ r^.textProgram) (r^.atlas) str
    return $ r & atlas .~ a


loadChar :: TextShaderProgram -> Atlas -> Char -> IO Atlas
loadChar tsp a c =
    let i = fromEnum c
    in
    case IM.lookup i (a^.atlasMap) of
        Just _  -> return a
        Nothing -> loadCharacter tsp a c


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
        Just (FontChar (w,h) (offX, offY) (NormGMetrics (bXp, bYp) advp)) -> do
            let Atlas _ tex _ pxS _ = r^.atlas

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

            -- Do some standard GL texture stuffs and render our quad with
            -- the char's texture.
            texture Texture2D $= Enabled
            activeTexture $= TextureUnit 0
            textureBinding Texture2D $= Just tex
            r^.textProgram.tShader.setModelview $ concat mv'
            r^.textProgram.renderTexQuad
            return (x + a, y)


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



    let verts = [ 0, 0
                , 1, 0
                , 1, 1
                , 0, 0
                , 1, 1
                , 0, 1
                ] :: [GLfloat]
        uvs    = verts
        size'  = length verts * sizeOf (undefined :: Float)
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

