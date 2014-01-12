module Graphics.Text.Font where

import Control.Monad
import Graphics.Rendering.OpenGL hiding (bitmap)
import Graphics.Rendering.FreeType.Internal
import qualified Graphics.Rendering.FreeType.Internal.BitmapSize as BS
import Graphics.Rendering.FreeType.Internal.PrimitiveTypes
import Graphics.Rendering.FreeType.Internal.Library
import Graphics.Rendering.FreeType.Internal.FaceType
import Graphics.Rendering.FreeType.Internal.Face
import Graphics.Rendering.FreeType.Internal.GlyphSlot
import Foreign
import Foreign.Marshal.Alloc
import Foreign.C.String
import Foreign.Storable
import Graphics.Rendering.FreeType.Internal.Bitmap
import Foreign.Marshal.Array
import Graphics.Texture.Load
import Graphics.Utils
import Data.Int
import Data.Word
import Data.List
import Codec.Picture


loadCharacter :: FilePath -> Char -> Int -> IO TextureObject
loadCharacter path char px = do
    -- FreeType (http://freetype.org/freetype2/docs/tutorial/step1.html)
    ft <- freeType
    -- Get the Ubuntu Mono fontface.
    ff <- fontFace ft path
    runFreeType $ ft_Set_Pixel_Sizes ff (fromIntegral px) 0
    -- Get the unicode char index.
    chNdx <- ft_Get_Char_Index ff $ fromIntegral $ fromEnum char

    -- Load the glyph into freetype memory.
    runFreeType $ ft_Load_Glyph ff chNdx 0

    -- Get the GlyphSlot.
    slot <- peek $ glyph ff

    -- Number of glyphs
    n <- peek $ num_glyphs ff
    putStrLn $ "glyphs:" ++ show n

    fmt <- peek $ format slot
    putStrLn $ "glyph format:" ++ glyphFormatString fmt

    putStr "Sizes:"
    numSizes <- peek $ num_fixed_sizes ff
    sizesPtr <- peek $ available_sizes ff 
    sizes <- forM [0 .. numSizes-1] $ \i ->
        peek $ sizesPtr `plusPtr` fromIntegral i :: IO BS.FT_Bitmap_Size
    print sizes
        

    l <- peek $ bitmap_left slot
    t <- peek $ bitmap_top slot
    putStrLn $ concat [ "left:"
                      , show l 
                      , "\ntop:" 
                      , show t
                      ]

    runFreeType $ ft_Render_Glyph slot ft_RENDER_MODE_NORMAL
    -- Get the char bitmap.
    bmp <- peek $ bitmap slot
    putStrLn $ concat ["width:"
                      , show $ width bmp
                      , " rows:"
                      , show $ rows bmp
                      , " pitch:"
                      , show $ pitch bmp
                      , " num_grays:"
                      , show $ num_grays bmp
                      , " pixel_mode:"
                      , show $ pixel_mode bmp
                      , " palette_mode:"
                      , show $ palette_mode bmp
                      ]

    let w  = fromIntegral $ width bmp
        h  = fromIntegral $ rows bmp
        w' = fromIntegral w
        h' = fromIntegral h
        p  = 4 - w `mod` 4
        nw = p + fromIntegral w'

    putStrLn $ "padding by " ++ show p

    -- Get the raw bitmap data.
    bmpData <- peekArray (w*h) $ buffer bmp
    
    let data' = addPadding p w 0 bmpData

    -- Set the texture params on our bound texture.
    texture Texture2D $= Enabled

    -- Generate an opengl texture.
    tex <- newBoundTexUnit 0
    printError

    putStrLn "Buffering glyph bitmap into texture."
    withArray data' $ \ptr -> texImage2D
        Texture2D
        NoProxy
        0
        R8
        (TextureSize2D (fromIntegral nw) h')
        0
        (PixelData Red UnsignedByte ptr)
    printError

    putStrLn "Texture loaded."
    textureFilter   Texture2D   $= ((Linear', Nothing), Linear')
    textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
    textureWrapMode Texture2D T $= (Repeated, ClampToEdge)

    return tex


addPadding :: Int -> Int -> a -> [a] -> [a]
addPadding _ _ _ [] = []
addPadding amt width val xs = a ++ b ++ c 
    where a = take width xs 
          b = replicate amt val 
          c = addPadding amt width val (drop width xs)


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


