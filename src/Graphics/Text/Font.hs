module Graphics.Text.Font where

import           Control.Monad
import           Control.Monad.State
import           Control.Lens
import           Graphics.Rendering.OpenGL.Raw.ARB.FramebufferObject
import           Graphics.Rendering.OpenGL hiding (bitmap)
import           Graphics.Rendering.FreeType.Internal
import           Graphics.Rendering.FreeType.Internal.PrimitiveTypes
import           Graphics.Rendering.FreeType.Internal.Library
import           Graphics.Rendering.FreeType.Internal.FaceType
import           Graphics.Rendering.FreeType.Internal.Face
import           Graphics.Rendering.FreeType.Internal.Bitmap as BM
import           Graphics.Rendering.FreeType.Internal.GlyphSlot as GS
import           Graphics.Rendering.FreeType.Internal.GlyphMetrics as GM
import           Data.Ratio
import           Data.IntMap as IM
import           Foreign
import           Foreign.C.String
import           Graphics.Texture.Load
import           Graphics.Utils
import           Graphics.Types
import           Graphics.Math
import           Graphics.Text.Shader

-- | Loads a character into our atlas.
-- It does so by rendering a glyph to a texture then draws the original
-- atlas and the glyph texture into a framebuffer that is used as the new
-- atlas.
--
-- https://developer.apple.com/library/ios/documentation/3ddrawing/conceptual/opengles_programmingguide/WorkingwithEAGLContexts/WorkingwithEAGLContexts.html#//apple_ref/doc/uid/TP40008793-CH103-SW6
loadCharacter :: TextShaderProgram -> Atlas -> Char -> IO Atlas
loadCharacter tsp a char = do
    let fp       = _atlasFontFilePath a
        px       = _atlasPxSize a
        (aW, aH) = _atlasTextureSize a
        aTex     = _atlasTextureObject a

    -- Render the glyph into a seperate texture.
    (charTex, (FontChar (gW, gH) gOffset gMtrx)) <- texturizeGlyphOfEnum fp px char

    let w  = aW + gW
        h  = max aH gH
        w' = fromIntegral w
        h' = fromIntegral h

    -- Create the destination texture, and attach it to the framebuffer’s
    -- color attachment point.
    --rowAlignment Unpack $= 1
    tex <- genObjectName
    texture Texture2D $= Enabled
    textureBinding Texture2D $= Just tex
    textureFilter Texture2D $= ((Linear', Just Linear'), Linear')
    textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
    textureWrapMode Texture2D T $= (Repeated, ClampToEdge)

    texImage2D
        Texture2D
        NoProxy
        0
        R8
        (TextureSize2D w' h')
        0
        (PixelData Red UnsignedByte nullPtr)

    -- Create a framebuffer render target.
    fb <- genObjectName
    bindFramebuffer Framebuffer $= fb
    framebufferTexture2D Framebuffer (ColorAttachment 0) Texture2D tex 0

    drawBuffers $= [FBOColorAttachment 0]
    -- Test for completeness.
    status <- glCheckFramebufferStatus gl_FRAMEBUFFER

    unless (status == gl_FRAMEBUFFER_COMPLETE) $
        print $ if status == gl_FRAMEBUFFER_INCOMPLETE_ATTACHMENT
                  then "incomplete attachment"
                  else show status

    -- Draw the two textures next to each other to form our new
    -- atlas.
    let vs x y ww hh = [ x, y
                       , x + ww, y
                       , x + ww, y + hh
                       , x, y + hh
                       ]
        aW'  = fromIntegral aW :: GLfloat
        aH'  = fromIntegral aH
        avs  = vs 0 0 aW' aH'
        gvs  = vs aW' 0 aW' aH'
        auvs = vs 0 0 1 1 :: [GLfloat]
        guvs = auvs
        w''  = fromIntegral w'
        h''  = fromIntegral h'
        pj   = orthoMatrix 0 w'' 0 h'' 0 1
        mv   = identityN 4
        scl  = scaleMatrix3d w'' h'' 1

    clear [ColorBuffer]
    viewport $= (Position 0 0, Size w' h')
    currentProgram $= (Just $ tsp^.tShader.program)
    tsp^.setSampler $ Index1 0
    tsp^.setTextColor $ Color4 1 0 0 1
    tsp^.tShader.setProjection $ concat pj
    tsp^.tShader.setModelview $ concat $ mv `multiply` scl

    -- Buffer and draw verts and uvs
    forM_ [(avs,auvs,aTex),(gvs,guvs,charTex)] $ \(verts,uvs,t) -> do
        -- Bind and buffer verts.
        [i,j] <- genObjectNames 2
        bindVBO i vertDescriptor $ AttribLocation 0
        withArray avs $ \ptr ->
            bufferData ArrayBuffer $= (fromIntegral $ length verts, ptr, StaticDraw)
        -- Bind and buffer uvs.
        bindVBO j uvDescriptor $ AttribLocation 1
        withArray uvs $ \ptr ->
            bufferData ArrayBuffer $= (fromIntegral $ length uvs, ptr, StaticDraw)
        -- Bind our texture.
        texture Texture2D $= Enabled
        activeTexture $= TextureUnit 0
        textureBinding Texture2D $= Just t
        drawArrays TriangleFan 0 4
        bindBuffer ArrayBuffer $= Nothing
        deleteObjectNames [i,j]

    currentProgram $= Nothing
    bindFramebuffer Framebuffer $= defaultFramebufferObject

    deleteObjectName fb
    deleteObjectNames [aTex,charTex]

    printError

    -- Update our atlas.
    return $ flip execState a $ do
        atlasMap %= IM.insert (fromEnum char) (FontChar (gW,gH) gOffset gMtrx)
        atlasTextureObject .= tex
        atlasTextureSize .= (w,h)


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


