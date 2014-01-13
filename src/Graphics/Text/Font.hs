module Graphics.Text.Font where

import           Control.Monad
import           Control.Lens
import           Graphics.Rendering.OpenGL hiding (bitmap)
import           Graphics.Rendering.FreeType.Internal
import           Graphics.Rendering.FreeType.Internal.PrimitiveTypes
import           Graphics.Rendering.FreeType.Internal.Library
import           Graphics.Rendering.FreeType.Internal.FaceType
import           Graphics.Rendering.FreeType.Internal.Face
import           Graphics.Rendering.FreeType.Internal.GlyphSlot
import           Foreign
import           Foreign.C.String
import           Graphics.Rendering.FreeType.Internal.Bitmap
import           Graphics.Texture.Load
import           Graphics.Utils
import           Graphics.Types
import qualified Graphics.Rendering.FreeType.Internal.BitmapSize as BS
import qualified Data.IntMap as IM

loadCharacter :: Atlas -> Char -> Int -> IO Atlas
loadCharacter a char texUnit = do
    let ff = _atlasFontFace a
        cNdx = fromEnum char

    -- Get the unicode char index.
    glyphNdx <- ft_Get_Char_Index ff $ fromIntegral cNdx

    loadGlyph a glyphNdx cNdx texUnit


loadGlyph :: Atlas -> FT_UInt -> Int -> Int -> IO Atlas
loadGlyph a glyphNdx cNdx texUnit = do
    let ft = _atlasFreeType a
        ff = _atlasFontFace a

    -- Load the glyph into freetype memory.
    runFreeType $ ft_Load_Glyph ff glyphNdx 0

    -- Get the GlyphSlot.
    slot <- peek $ glyph ff

    -- Get the char bitmap.
    runFreeType $ ft_Render_Glyph slot ft_RENDER_MODE_NORMAL
    bmp <- peek $ bitmap slot

    let w  = fromIntegral $ width bmp
        h  = fromIntegral $ rows bmp
        w' = fromIntegral w
        h' = fromIntegral h

    -- Set the texture params on our bound texture.
    texture Texture2D $= Enabled

    -- Set the alignment to 1 byte.
    rowAlignment Unpack $= 1

    -- Generate an opengl texture.
    tex <- newBoundTexUnit texUnit
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
    return $ a & atlasMap %~ IM.insert cNdx FontChar { _fcTextureObject = tex
                                                     , _fcTextureSize = (w,h)
                                                     , _fcGlyphIndex = glyphNdx
                                                     }


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


