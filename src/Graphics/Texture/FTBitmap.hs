module Graphics.Texture.FTBitmap where

import Graphics.Rendering.FreeType.Internal.Bitmap
import Graphics.Rendering.OpenGL
import Codec.Picture
import Foreign.Marshal.Array
import Graphics.Texture.Load
import Data.Int

bitmapToTexture :: FT_Bitmap -> IO ()
bitmapToTexture bmp = do
    let w = fromIntegral $ rows bmp
        h = fromIntegral $ width bmp
    -- Get the bitmap as an array of Int8.
    raw <- peekArray (fromIntegral (rows bmp) * fromIntegral (width bmp)) $ buffer bmp
    let pixels = map fromIntegral raw :: [Int8]
    print pixels
    texture Texture2D $= Enabled
    tex <- newBoundTexUnit 0
    withArray pixels $ \ptr -> texImage2D
        Texture2D
        -- No proxy
        NoProxy
        -- No mipmaps
        0
        -- Internal storage @ 8bit grey/redscale
        R8
        -- Size of the image.
        (TextureSize2D w h)
        -- No borders
        0
        -- Pixel data in bytes, grey/redscale
        (PixelData Red Int ptr)
    printError

