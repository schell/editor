module Graphics.Utils where

import           Graphics.Rendering.OpenGL
import           Control.Monad
import           Foreign.Ptr
import           System.IO (hPutStrLn, stderr)
import           System.Exit (exitFailure)


-- | Prints any accumulated OpenGL errors.
printError :: IO ()
printError = get errors >>= mapM_ (hPutStrLn stderr . ("GL: "++) . show)


-- | Renders the IO () action into a framebuffer texture.
renderToTexture :: (GLsizei, GLsizei) -> PixelInternalFormat -> IO () -> IO TextureObject
renderToTexture (w,h) fmt ioF = do
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
-- Assumes (0,0) is the upper left, y increasing downward.
quad :: Num a => a -> a -> a -> a -> [a]
quad x y w h = [x, y, x + w, y, x + w, y + h, x, y, x + w, y + h, x, y + h]


-- | Returns uvs for a two-tri quad.
-- Assumes (0,0) is the lower left, y incresing upward.
texQuad :: Num a => a -> a -> a -> a -> [a]
texQuad x y w h =
    [ x, y + h
    , x + w , y + h
    , x + w, y
    , x, y + h
    , x + w, y
    , x, y
    ]


