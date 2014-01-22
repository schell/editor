{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Graphics.Rendering.Shader.Text (
    module T,
    makeTextShaderProgram,
    bindAndBufferVertsUVs
) where

import           Graphics.Utils
import           Graphics.Rendering.Shader.Utils
import           Graphics.Rendering.Shader.Text.Types as T
import           Graphics.Rendering.OpenGL hiding (Bitmap, Matrix)
import           Graphics.Rendering.OpenGL.Raw (glUniformMatrix4fv)
import           Foreign
import           Graphics.Rendering.Shader.TH 
import qualified Data.ByteString as B


-- | Compiles, validates and returns a shader for rendering text with.
makeTextShaderProgram :: IO TextShaderProgram
makeTextShaderProgram = do
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

    return TextShaderProgram { _program = p
                             , _setProjection = updatePJ
                             , _setModelview = updateMV
                             , _setSampler = updateSampler
                             , _setTextColor = updateColor
                             }


-- | GLSL Source code for a text vertex shader.
vertSrc :: B.ByteString
vertSrc = $(embedFile "shaders/textshader.vert") 

-- | GLSL Source code for a text fragment shader.
fragSrc :: B.ByteString
fragSrc = $(embedFile "shaders/textshader.frag") 

-- | Vertex descriptor for a tex vertex shader.
vertDescriptor :: VertexArrayDescriptor [Float]
vertDescriptor = VertexArrayDescriptor 2 Float 0 nullPtr


-- | UV descriptor for a tex vertex shader.
uvDescriptor :: VertexArrayDescriptor [Float]
uvDescriptor = vertDescriptor


-- | Binds and buffers vertices and uv coords to be used with a text
-- shader. This assumes that a text shader program is the current program.
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

