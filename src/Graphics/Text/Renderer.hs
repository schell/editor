{-# LANGUAGE OverloadedStrings #-}
module Graphics.Text.Renderer (
    module T,
    initTextRenderer
) where

import           Graphics.Utils
import           Graphics.Types as T
import           Graphics.Text.Font
import           Graphics.Texture.Load
import           Graphics.Rendering.OpenGL
import           Graphics.Rendering.OpenGL.Raw
import           Codec.Picture
import           Control.Monad
import           Data.Maybe
import           System.Exit            (exitFailure)
import           Data.Vector.Storable   (unsafeWith)
import           Foreign.Marshal.Array  (withArray)
import           Foreign.Storable       ( sizeOf )
import           Foreign.Ptr            (nullPtr)
import qualified Data.ByteString as B


vertSrc :: B.ByteString
vertSrc = B.intercalate "\n"
    [ "attribute vec2 position;"
    , "attribute vec2 uv;"

    , "varying vec2 vTex; "

    , "uniform mat4 modelview;"
    , "uniform mat4 projection;"

    , "void main () {"
    , "    vTex = uv;"
    , "    gl_Position = projection * modelview * vec4(position, 0.0, 1.0);"
    , "}"
    ]


fragSrc :: B.ByteString
fragSrc = B.intercalate "\n"
    [ "varying vec2 vTex;"

    , "uniform sampler2D sampler;"
    , "uniform vec4 color;"

    , "void main() {"
    , "    vec4 tc = texture2D(sampler, vec2(vTex.s,vTex.t));"
    , "    gl_FragColor = tc;//vec4(tc.r*color.r,tc.g*color.g,tc.b*color.b,tc.a*color.a);"
    , "}"
    ]


vertDescriptor :: VertexArrayDescriptor [Float]
vertDescriptor = VertexArrayDescriptor 2 Float 0 nullPtr


uvDescriptor :: VertexArrayDescriptor [Float]
uvDescriptor = vertDescriptor


initTextRenderer :: FilePath -> IO TextRenderer
initTextRenderer font = do
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

    -- Load the font atlas.
    Just t <- initTexture font 0
    --t <- loadFontAtlas font (128,128)

    let drawText _ = do --let
                        --    verts  = stringToVerts s
                        --    uvs    = stringToUVs s
                        let verts = [ 0, 0
                                    , 1, 0
                                    , 1, 1
                                    , 0, 0
                                    , 1, 1
                                    , 0, 1
                                    ] :: [GLfloat]
                            uvs    = verts
                            size   = length verts * sizeOf (undefined :: Float)
                        currentProgram $= Just p
                        [i,j] <- genObjectNames 2
                        -- Buffer the verts
                        bindVBO i vertDescriptor $ AttribLocation 0
                        withArray verts $ \ptr ->
                            bufferData ArrayBuffer $= (fromIntegral size, ptr, StaticDraw)
                        -- Buffer the uvs
                        bindVBO j uvDescriptor $ AttribLocation 1
                        withArray uvs $ \ptr ->
                            bufferData ArrayBuffer $= (fromIntegral size, ptr, StaticDraw)

                        texture Texture2D $= Enabled
                        activeTexture $= TextureUnit 0
                        textureBinding Texture2D $= Just t
                        bindVBO i vertDescriptor $ AttribLocation 0
                        bindVBO j uvDescriptor $ AttribLocation 1
                        drawArrays Triangles 0 6--(6*length s)
                        bindBuffer ArrayBuffer $= Nothing
                        deleteObjectNames [i,j]
                        printError

    return TextRenderer { _textProgram    = RndrProgram3D { _program = p
                                                          , _setModelview  = updateMV
                                                          , _setProjection = updatePJ
                                                          }
                        , _setSampler = updateSampler
                        , _setTextColor = updateColor
                        , _drawText = drawText
                        }

