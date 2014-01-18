module Graphics.Text.Font where

import           Control.Monad
import           Graphics.Rendering.OpenGL hiding (bitmap, Matrix)
import           Graphics.Rendering.FreeType.Internal
import           Graphics.Rendering.FreeType.Internal.PrimitiveTypes
import           Graphics.Rendering.FreeType.Internal.Library
import           Graphics.Rendering.FreeType.Internal.FaceType
import           Graphics.Rendering.FreeType.Internal.Face
import           Graphics.Rendering.FreeType.Internal.Bitmap as BM
import           Graphics.Rendering.FreeType.Internal.GlyphSlot as GS
import           Graphics.Rendering.FreeType.Internal.GlyphMetrics as GM
import           Data.Ratio
import           Foreign
import           Foreign.C.String
import           Graphics.Texture.Load
import           Graphics.Utils
import           Graphics.Types


texturizeGlyphOfEnum :: Enum a => FilePath -> Int -> a -> IO (TextureObject, FontChar)
texturizeGlyphOfEnum file px enm = do
    ft <- freeType
    ff <- fontFace ft file

    -- Set the size of the glyph
    runFreeType $ ft_Set_Pixel_Sizes ff 0 $ fromIntegral px

    ndx <- case fromEnum enm of
               -- Since we load the missing glyph with character '\NUL' 
               -- and the glyph index of '\NUL' can be non-zero
               -- we have to check here and explicitly get the missing 
               -- glyph.
               0 -> return 0 
               e -> ft_Get_Char_Index ff $ fromIntegral e

    -- Load the glyph into freetype memory.
    runFreeType $ ft_Load_Glyph ff ndx 0

    -- Get the GlyphSlot.
    slot <- peek $ glyph ff

    -- Get the char bitmap.
    runFreeType $ ft_Render_Glyph slot ft_RENDER_MODE_NORMAL
    bmp <- peek $ bitmap slot

    let w  = fromIntegral $ BM.width bmp
        h  = fromIntegral $ rows bmp
        w' = fromIntegral w
        h' = fromIntegral h


    activeTexture $= TextureUnit 0

    -- Set the texture params on our bound texture.
    texture Texture2D $= Enabled

    -- Set the alignment to 1 byte.
    rowAlignment Unpack $= 1

    -- Generate an opengl texture.
    tex <- newBoundTexUnit 0
    printError

    texImage2D
        Texture2D
        NoProxy
        0
        R8
        (TextureSize2D w' h')
        0
        (PixelData Red UnsignedByte $ buffer bmp)
    printError

    textureFilter   Texture2D   $= ((Linear', Nothing), Linear')
    textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
    textureWrapMode Texture2D T $= (Repeated, ClampToEdge)

    nGlyphMetrics <- fmap normalizeGlyphMetrics $ peek $ GS.metrics slot

    runFreeType $ ft_Done_FreeType ft

    return (tex, FontChar { _fcTextureSize = (w, h)
                          , _fcTextureOffset = (0, 0)
                          , _fcNormMetrics = nGlyphMetrics
                          })


addPadding :: Int -> Int -> a -> [a] -> [a]
addPadding _ _ _ [] = []
addPadding amt w val xs = a ++ b ++ c
    where a = take w xs
          b = replicate amt val
          c = addPadding amt w val (drop w xs)


glyphFormatString :: FT_Glyph_Format -> String
glyphFormatString fmt
    | fmt == ft_GLYPH_FORMAT_COMPOSITE = "ft_GLYPH_FORMAT_COMPOSITE"
    | fmt == ft_GLYPH_FORMAT_OUTLINE = "ft_GLYPH_FORMAT_OUTLINE"
    | fmt == ft_GLYPH_FORMAT_PLOTTER = "ft_GLYPH_FORMAT_PLOTTER"
    | fmt == ft_GLYPH_FORMAT_BITMAP = "ft_GLYPH_FORMAT_BITMAP"
    | otherwise = "ft_GLYPH_FORMAT_NONE"


normalizeGlyphMetrics :: FT_Glyph_Metrics -> NormalizedGlyphMetrics
normalizeGlyphMetrics m = NormGMetrics bxy adv
    where bX  = fromIntegral (horiBearingX m) % fromIntegral (GM.width m)
          bY  = fromIntegral (horiBearingY m) % fromIntegral (GM.height m)
          bxy = (bX,bY)
          adv = fromIntegral (horiAdvance m) % fromIntegral (GM.width m)


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


