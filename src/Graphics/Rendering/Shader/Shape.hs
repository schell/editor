{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Graphics.Rendering.Shader.Shape (
    module T,
    makeShapeShaderProgram,
    bindAndBufferVertsUVs,
    bindAndBufferVertsColors
) where

import           Graphics.Utils
import           Graphics.Rendering.Shader.Utils
import           Graphics.Rendering.Shader.Shape.Types as T
import           Graphics.Rendering.OpenGL hiding (Bitmap, Matrix)
import           Graphics.Rendering.OpenGL.Raw (glUniformMatrix4fv)
import           Foreign
import           Control.Monad
import           Graphics.Rendering.Shader.TH
import qualified Data.ByteString as B


-- | Compiles, validates and returns a shader for rendering text with.
makeShapeShaderProgram :: IO ShapeShaderProgram
makeShapeShaderProgram = do
    v <- makeShader VertexShader vertSrc
    f <- makeShader FragmentShader fragSrc

    p <- makeProgram [v,f] [ ("position", positionLocation)
                           , ("color", colorLocation)
                           , ("uv", uvLocation)
                           ]
    currentProgram $= Just p
    printError

    UniformLocation mv <- get $ uniformLocation p "modelview"
    UniformLocation pj <- get $ uniformLocation p "projection"

    let updateMV mat = withArray mat $ \ptr ->
                           glUniformMatrix4fv mv 1 1 ptr
        updatePJ mat = withArray mat $ \ptr ->
                           glUniformMatrix4fv pj 1 1 ptr

    sLoc <- get $ uniformLocation p "sampler"
    tLoc <- get $ uniformLocation p "isTextured"
    let updateSampler s = uniform sLoc $= s

    return ShapeShaderProgram { _program = p
                              , _setProjection = updatePJ
                              , _setModelview = updateMV
                              , _setSampler = updateSampler
                              , _setIsTextured = updateIsTextured tLoc
                              }


-- | Updates the shader to accept either uv coords if textured or color
-- values if not. Assumes a shape shader program is set as the current
-- program.
updateIsTextured :: UniformLocation -> Bool -> IO ()
updateIsTextured uloc isTextured = do
    when isTextured $ do
        vertexAttribArray colorLocation $= Disabled
        vertexAttribArray uvLocation $= Enabled

    unless isTextured $ do
        vertexAttribArray colorLocation $= Enabled
        vertexAttribArray uvLocation $= Disabled

    uniform uloc $= (Index1 $ if isTextured then 1 else 0 :: GLint)


-- | GLSL Source code for a shape vertex shader.
vertSrc :: B.ByteString
vertSrc = $(embedFile "shaders/shapeshader.vert")


-- | GLSL Source code for a shape fragment shader.
fragSrc :: B.ByteString
fragSrc = $(embedFile "shaders/shapeshader.frag")


positionLocation :: AttribLocation
positionLocation = AttribLocation 0


colorLocation :: AttribLocation
colorLocation = AttribLocation 1


uvLocation :: AttribLocation
uvLocation = AttribLocation 2


-- | Vertex descriptor for a shape vertex shader.
vertDescriptor :: VertexArrayDescriptor [Float]
vertDescriptor = VertexArrayDescriptor 2 Float 0 nullPtr


-- | UV descriptor for a shape vertex shader.
uvDescriptor :: VertexArrayDescriptor [Float]
uvDescriptor = vertDescriptor


-- | Color descriptor for a shape vertex shader.
colorDescriptor :: VertexArrayDescriptor [Float]
colorDescriptor = VertexArrayDescriptor 4 Float 0 nullPtr


-- | Binds and buffers vertices and uv coords to be used with a text
-- shader. This assumes that a text shader program is the current program.
bindAndBufferVertsUVs :: [GLfloat] -> [GLfloat] -> IO (BufferObject, BufferObject)
bindAndBufferVertsUVs vts uvs = do
    [i,j] <- genObjectNames 2

    bindVBO i vertDescriptor positionLocation
    withArray vts $ \ptr -> do
        bufferData ArrayBuffer $= (sizeOfList vts, ptr, StaticDraw)

    bindVBO j uvDescriptor uvLocation
    withArray uvs $ \ptr -> do
        bufferData ArrayBuffer $= (sizeOfList uvs, ptr, StaticDraw)

    return (i,j)


bindAndBufferVertsColors :: [GLfloat] -> [GLfloat] -> IO (BufferObject, BufferObject)
bindAndBufferVertsColors vts cs = do
    [i,j] <- genObjectNames 2

    bindVBO i vertDescriptor positionLocation
    withArray vts $ \ptr -> do
        bufferData ArrayBuffer $= (sizeOfList vts, ptr, StaticDraw)

    bindVBO j colorDescriptor colorLocation
    withArray cs $ \ptr -> do
        bufferData ArrayBuffer $= (sizeOfList cs, ptr, StaticDraw)

    return (i,j)
