module Graphics.Text.Font where

import           Control.Monad
import           Control.Monad.State hiding (get)
import           Control.Lens
import           Graphics.Rendering.OpenGL hiding (bitmap, Matrix)
import           Graphics.Rendering.FreeType.Internal
import           Graphics.Rendering.FreeType.Internal.PrimitiveTypes
import           Graphics.Rendering.FreeType.Internal.Library
import           Graphics.Rendering.FreeType.Internal.FaceType
import           Graphics.Rendering.FreeType.Internal.Face
import           Graphics.Rendering.FreeType.Internal.Bitmap as BM
import           Graphics.Rendering.FreeType.Internal.GlyphSlot as GS
import           Graphics.Rendering.FreeType.Internal.GlyphMetrics as GM
import           System.Exit
import           Data.Ratio
import           Data.IntMap as IM
import           Foreign
import           Foreign.C.String
import           Graphics.Texture.Load
import           Graphics.Utils
import           Graphics.Types
import           Graphics.Math

-- | Loads a character into our atlas.
-- It does so by rendering a glyph to a texture then draws the original
-- atlas and the glyph texture into a framebuffer that is used as the new
-- atlas.
loadCharacter :: TextRenderer -> Char -> IO TextRenderer
loadCharacter r char = do
    let fp       = r^.atlas.atlasFontFilePath
        px       = r^.atlas.atlasPxSize
        (aW, aH) = r^.atlas.atlasTextureSize
        aTex     = r^.atlas.atlasTextureObject

    -- Render the glyph into a seperate texture.
    (charTex, (FontChar (gW, gH) gOffset gMtrx)) <- texturizeGlyphOfEnum fp px char

    let w  = aW + gW
        h  = max aH gH
        w' = fromIntegral w
        h' = fromIntegral h

    fb <- genObjectName
    bindFramebuffer Framebuffer $= fb

    tex <- genObjectName
    textureBinding Texture2D $= Just tex
    textureFilter Texture2D $= ((Linear', Nothing), Linear')
    texImage2D
        Texture2D
        NoProxy
        0
        R8
        (TextureSize2D w' h')
        0
        (PixelData RGB UnsignedByte nullPtr)

    framebufferTexture2D Framebuffer (ColorAttachment 0) Texture2D tex 0

    status <- get $ framebufferStatus Framebuffer
    unless (status == Complete) $ do
        print status
        exitFailure

    -- Now render business as usual.
    let pj  = orthoMatrix 0 (fromIntegral w) 0 (fromIntegral h) 0 1 :: Matrix GLfloat
        aW' = fromIntegral aW
        aH' = fromIntegral aH
        gW' = fromIntegral gW
        gH' = fromIntegral gH

    clearColor $= Color4 0 0 0 1
    clear [ColorBuffer, DepthBuffer]
    viewport $= (Position 0 0, Size w' h')
    currentProgram $= (Just $ r^.textProgram.tShader.program)
    r^.textProgram.tShader.setProjection $ concat pj
    renderTex r aTex (0,0) (aW', aH')
    renderTex r charTex (aW', 0) (gW', gH')

    bindFramebuffer Framebuffer $= defaultFramebufferObject

    deleteObjectName charTex
    deleteObjectName aTex
    deleteObjectName fb

    -- Update our atlas.
    return $ flip execState r $ do
        atlas.atlasMap %= IM.insert (fromEnum char) (FontChar (gW,gH) gOffset gMtrx)
        atlas.atlasTextureObject .= tex
        atlas.atlasTextureSize .= (w,h)


renderTex :: TextRenderer -> TextureObject -> (GLfloat, GLfloat) -> (GLfloat, GLfloat) -> IO ()
renderTex r t (x,y) (w,h) = do
    let scl = scaleMatrix3d w h 1 :: Matrix GLfloat
        tns = translationMatrix3d x y 0 :: Matrix GLfloat
        mv = identityN 4 `multiply` tns `multiply` scl
    -- aTex
    r^.textProgram.tShader.setModelview $ concat mv
    r^.textProgram.setSampler $ Index1 0
    r^.textProgram.setTextColor $ Color4 1 0 0 1
    texture Texture2D $= Enabled
    activeTexture $= TextureUnit 0
    textureBinding Texture2D $= Just t
    r^.textProgram.renderTexQuad


texturizeGlyphOfEnum :: Enum a => FilePath -> Int -> a -> IO (TextureObject, FontChar)
texturizeGlyphOfEnum file px enm = do
    ft <- freeType
    ff <- fontFace ft file

    -- Set the size of the glyph
    runFreeType $ ft_Set_Pixel_Sizes ff 0 $ fromIntegral px

    -- Load the glyph into freetype memory.
    runFreeType $ ft_Load_Glyph ff (fromIntegral $ fromEnum enm) 0

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


