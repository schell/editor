module Graphics.Text.Font where

import Control.Monad
import Graphics.Rendering.OpenGL hiding (bitmap)
import Graphics.Rendering.FreeType.Internal
import Graphics.Rendering.FreeType.Internal.PrimitiveTypes
import Graphics.Rendering.FreeType.Internal.Library
import Graphics.Rendering.FreeType.Internal.FaceType
import Graphics.Rendering.FreeType.Internal.Face
import Graphics.Rendering.FreeType.Internal.GlyphSlot
import Foreign.Marshal.Alloc
import Foreign.C.String
import Foreign.Storable
import Graphics.Rendering.FreeType.Internal.Bitmap
import Foreign.Marshal.Array
import Graphics.Texture.Load
import Graphics.Utils
import Data.Int
import Data.List

bitmapToTexture :: FT_Bitmap -> IO TextureObject
bitmapToTexture bmp = do
    let w = fromIntegral $ rows bmp
        h = fromIntegral $ width bmp
    -- Get the bitmap as an array of Int8.
    raw <- peekArray (fromIntegral (rows bmp) * fromIntegral (width bmp)) $ buffer bmp
    let pixels = map fromIntegral raw :: [Int8]
    print (length pixels, w*h)
    texture Texture2D $= Enabled
    tex <- newBoundTexUnit 0
    printError
    putStrLn "Buffering glyph bitmap into texture."
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
        (PixelData Red UnsignedByte ptr)
    printError
    putStrLn "Texture loaded."
    return tex


loadFontAtlas :: FilePath -> (Int,Int) -> IO TextureObject
loadFontAtlas path (w, h) = do
    -- FreeType (http://freetype.org/freetype2/docs/tutorial/step1.html)
    ft <- freeType
    -- Get the Ubuntu Mono fontface.
    ff <- fontFace ft path
    runFreeType $ ft_Set_Pixel_Sizes ff (fromIntegral w) (fromIntegral h)
    -- Get the unicode char index.
    chNdx <- ft_Get_Char_Index ff 65
    -- Load the glyph into freetype memory.
    runFreeType $ ft_Load_Glyph ff chNdx 0
    -- Get the GlyphSlot.
    slot <- peek $ glyph ff
    fmt <- peek $ format slot
    putStrLn $ "glyph format:" ++ glyphFormatString fmt
    runFreeType $ ft_Render_Glyph slot ft_RENDER_MODE_NORMAL
    -- Get the char bitmap.
    bmp <- peek $ bitmap slot
    putStrLn $ concat [ "rows:"
                      , show $ rows bmp
                      , " width:"
                      , show $ width bmp
                      , " pitch:"
                      , show $ pitch bmp
                      , " num_grays:"
                      , show $ num_grays bmp
                      , " pixel_mode:"
                      , show $ pixel_mode bmp
                      , " palette_mode:"
                      , show $ palette_mode bmp
                      ]
    -- Set the texture params on our bound texture.
    textureFilter   Texture2D   $= ((Linear', Nothing), Linear')
    textureWrapMode Texture2D S $= (Repeated, Clamp)
    textureWrapMode Texture2D T $= (Repeated, Clamp)

    -- Generate an opengl texture.
    bitmapToTexture bmp


glyphFormatString :: FT_Glyph_Format -> String
glyphFormatString fmt
    | fmt == ft_GLYPH_FORMAT_COMPOSITE = "ft_GLYPH_FORMAT_COMPOSITE"
    | fmt == ft_GLYPH_FORMAT_OUTLINE = "ft_GLYPH_FORMAT_OUTLINE"
    | fmt == ft_GLYPH_FORMAT_PLOTTER = "ft_GLYPH_FORMAT_PLOTTER"
    | fmt == ft_GLYPH_FORMAT_BITMAP = "ft_GLYPH_FORMAT_BITMAP"
    | otherwise = "ft_GLYPH_FORMAT_NONE"



runFreeType :: IO FT_Error -> IO ()
runFreeType m = do
    r <- m
    unless (r == 0) $ fail $ "FreeType Error:" ++ show r


freeType :: IO FT_Library
freeType = alloca $ \p -> do
    runFreeType $ ft_Init_FreeType p
    peek p


fontFace :: FT_Library -> FilePath -> IO FT_Face
fontFace ft fp = withCString fp $ \str ->
    alloca $ \ptr -> do
        runFreeType $ ft_New_Face ft str 0 ptr
        peek ptr


