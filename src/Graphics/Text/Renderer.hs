{-# LANGUAGE OverloadedStrings #-}
module Graphics.Text.Renderer (
    module T,
    initTextRenderer,
    drawTextAt,
    drawTextAt',
    loadCharMap,
) where

import           Graphics.Utils
import           Graphics.Math
import           Graphics.Text.Character
import           Graphics.Types as T
import           Graphics.Text.Font
import           Graphics.Text.Shader
import           Graphics.Rendering.OpenGL hiding (Bitmap, Matrix)
import           Graphics.Rendering.OpenGL.Raw
import           Control.Monad
import           Control.Lens
import           Data.Monoid
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


drawTextAt :: TextRenderer -> PenPosition -> String -> IO ()
drawTextAt r (x,y) = foldM_ foldCharacter (x,y)
    where foldCharacter (_,y') '\n' = return (x, y' + fromIntegral (r^.atlas.atlasPxSize))
          foldCharacter p c          = drawChar r p c


drawTextAt' :: TextRenderer -> PenPosition -> String -> IO ()
drawTextAt' r pen s = do
    let (BufferAcc _ (vs,uvs) _) = geometryForString (BufferAcc (r^.atlas) mempty pen) s

    (i,j) <- bindAndBufferVertsUVs vs uvs
    texture Texture2D $= Enabled
    activeTexture $= TextureUnit 0
    textureBinding Texture2D $= Just (r^.atlas.atlasTextureObject)
    r^.textProgram.setSampler $ Index1 0
    r^.textProgram.tShader.setModelview $ concat $ identityN 4
    drawArrays Triangles 0 $ fromIntegral $ 6 * length s
    bindBuffer ArrayBuffer$= Nothing
    deleteObjectNames [i,j]


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



    let verts = quad 0 0 1 1 :: [GLfloat]
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

