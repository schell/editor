module Graphics.Utils where

import           Graphics.Rendering.OpenGL
import           Control.Monad
import           Foreign.Ptr
import           Foreign
import           System.IO (hPutStrLn, stderr)
import           System.Exit (exitFailure)
import qualified Data.ByteString as B


printError :: IO ()
printError = get errors >>= mapM_ (hPutStrLn stderr . ("GL: "++) . show)


sizeOfList :: [GLfloat] -> GLsizeiptr
sizeOfList = fromIntegral . (* sizeOf (undefined :: GLfloat)) . length


makeShader :: ShaderType -> B.ByteString -> IO Shader
makeShader ty src = do
    s <- createShader ty
    shaderSourceBS s $= src
    compileShader s
    s'Ok <- get $ compileStatus s
    unless s'Ok $ do
        slog <- get $ shaderInfoLog s
        putStrLn $ "Log:" ++ slog
        exitFailure
    printError
    return s


makeProgram :: [Shader] -> [(String, AttribLocation)] -> IO Program
makeProgram shaders attributes = do
    p <- createProgram
    mapM_ (attachShader p) shaders
    mapM_ (\(name, loc) -> attribLocation p name $= loc) attributes
    linkProgram p
    p'Ok <- get $ linkStatus p
    validateProgram p
    status <- get $ validateStatus p
    unless (p'Ok && status) $ do
        plog <- get $ programInfoLog p
        putStrLn plog
        printError
        exitFailure
    return p


bindVBO :: BufferObject -> VertexArrayDescriptor a -> AttribLocation -> IO ()
bindVBO vbo dsc loc = do
    bindBuffer ArrayBuffer $= Just vbo
    vertexAttribPointer loc $= (ToFloat, dsc)
    vertexAttribArray loc $= Enabled


renderToTexture :: (GLsizei, GLsizei) -> PixelInternalFormat -> IO () -> IO TextureObject
renderToTexture (w,h) fmt ioF = do
    putStrLn $ "renderToTexture: " ++ show (w,h)
    fb <- genObjectName
    bindFramebuffer Framebuffer $= fb

    tex <- genObjectName
    textureBinding Texture2D $= Just tex
    textureFilter Texture2D $= ((Linear', Nothing), Linear')
    texImage2D
        Texture2D
        NoProxy
        0
        fmt
        (TextureSize2D w h)
        0
        (PixelData RGB UnsignedByte nullPtr)
    framebufferTexture2D Framebuffer (ColorAttachment 0) Texture2D tex 0

    status <- get $ framebufferStatus Framebuffer
    unless (status == Complete) $ do
        print status
        exitFailure

    ioF
    bindFramebuffer Framebuffer $= defaultFramebufferObject
    deleteObjectName fb
    return tex


-- | Returns vertices for a two-tri quad.
-- Assumes (0,0) is the upper left, y incresing downward.
quad :: Num a => a -> a -> a -> a -> [a]
quad x y w h = [x, y, x + w, y, x + w, y + h, x, y, x + w, y + h, x, y + h]


-- | Returns uvs for a two-tri quad.
-- Assums (0,0) is the lower left, y incresing upward.
texQuad :: Num a => a -> a -> a -> a -> [a]
texQuad x y w h =
    [ x, y + h
    , x + w , y + h
    , x + w, y
    , x, y + h
    , x + w, y
    , x, y
    ]


