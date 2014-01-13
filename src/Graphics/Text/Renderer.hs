{-# LANGUAGE OverloadedStrings #-}
module Graphics.Text.Renderer (
    module T,
    initTextRenderer,
    drawTextAt,
    loadCharMap,
    TextResource(..)
) where

import           Graphics.Utils
import           Graphics.Types as T
import           Graphics.Math
import           Graphics.Text.Font
import           Graphics.Texture.Load
import           Graphics.Rendering.OpenGL hiding (Bitmap, Matrix)
import           Graphics.Rendering.OpenGL.Raw
import           Graphics.Rendering.FreeType.Internal
import           Graphics.Rendering.FreeType.Internal.Face
import           Graphics.Rendering.FreeType.Internal.GlyphMetrics as GM
import           Graphics.Rendering.FreeType.Internal.GlyphSlot as GS
import           Graphics.Rendering.FreeType.Internal.Size as S
import           Graphics.Rendering.FreeType.Internal.SizeMetrics
import           Graphics.Rendering.FreeType.Internal.BBox
import           Codec.Picture
import           Control.Monad
import           Control.Lens
import           Data.Maybe
import           Data.Ratio
import           System.Directory
import           System.Exit            (exitFailure)
import           Data.Vector.Storable   (unsafeWith)
import           Foreign.Marshal.Array  (withArray)
import           Foreign.Storable       ( sizeOf )
import           Foreign
import           Foreign.Ptr
import           Foreign.C.Types
import qualified Data.ByteString as B
import qualified Data.IntMap as IM


data FontMetrics = FMtrx { _upem :: GLfloat
                         , _ppem :: GLfloat
                         , _ppfu :: GLfloat
                         , _bbox :: (GLfloat, GLfloat)
                         }


data GlyphMetrics = GMtrx { _bearing :: (GLfloat, GLfloat)
                          , _advance :: GLfloat
                          }


vertSrc :: B.ByteString
vertSrc = B.intercalate "\n"
    [ "attribute vec2 position;"
    , "attribute vec2 uv;"

    , "varying vec2 vTex; "

    , "uniform mat4 modelview;"
    , "uniform mat4 projection;"

    , "void main () {"
    , "    vTex = uv;"
    , "    gl_Position = projection * modelview * vec4(position, 0.0, 1.0);"
    , "}"
    ]


fragSrc :: B.ByteString
fragSrc = B.intercalate "\n"
    [ "varying vec2 vTex;"

    , "uniform sampler2D sampler;"
    , "uniform vec4 color;"

    , "void main() {"
    , "    vec4 tc = texture2D(sampler, vec2(vTex.s,vTex.t));"
    , "    gl_FragColor = vec4(color.r,color.g,color.b,tc.r);"
    , "}"
    ]


vertDescriptor :: VertexArrayDescriptor [Float]
vertDescriptor = VertexArrayDescriptor 2 Float 0 nullPtr


uvDescriptor :: VertexArrayDescriptor [Float]
uvDescriptor = vertDescriptor


recycleAtlas :: Atlas -> IO ()
recycleAtlas a = return ()


loadAtlas :: TextRenderer -> FilePath -> Int -> IO TextRenderer
loadAtlas r fp px = do
    recycleAtlas $ r^.atlas
    a <- initAtlas fp px
    return $ r & atlas .~ a


initAtlas :: FilePath -> Int -> IO Atlas
initAtlas fp px = do
    -- FreeType (http://freetype.org/freetype2/docs/tutorial/step1.html)
    ft <- freeType

    -- Get the fontface.
    ff <- fontFace ft fp
    let px' = fromIntegral px
    putStrLn $ "Got pixel size " ++ show px ++ ", setting font size to " ++ show px'
    runFreeType $ ft_Set_Pixel_Sizes ff 0 px'

    mu <- peek $ units_per_EM ff

    s  <- peek $ size ff

    -- Load the missing glyph as the first char.
    loadGlyph (Atlas ft ff px IM.empty) 0 0 0


loadCharMap :: TextRenderer -> FilePath -> String -> IO TextRenderer
loadCharMap r font str = do
    -- Load the font atlas for this string.
    a <- foldM (loadChar font) (r^.atlas) str
    return $ r & atlas .~ a


loadChar :: FilePath -> Atlas -> Char -> IO Atlas
loadChar f a c =
    let i = fromEnum c
    in
    case IM.lookup i (a^.atlasMap) of
        Just _  -> return a
        Nothing -> loadCharacter a c 0


drawTextAt :: TextRenderer -> (GLfloat, GLfloat) -> String -> IO ()
drawTextAt r (x,y) = foldM_ foldCharacter (x,y)
    where foldCharacter (x',y') '\n' = return (x, y' + fromIntegral (r^.atlas.atlasPxSize)) 
          foldCharacter p c          = drawCharacter r p c


drawCharacter :: TextRenderer -> (GLfloat, GLfloat) -> Char -> IO (GLfloat, GLfloat)
drawCharacter r (x,y) ' ' = 
    let mChar = IM.lookup 0 $ r^.atlas.atlasMap 
    in 
    case mChar of
        -- Worst case scenario we advance by the pixel size.
        Nothing -> return (x + fromIntegral (r^.atlas.atlasPxSize), y)
        Just (FontChar _ (w,_) ndx) -> do
            let Atlas _ ff _ _ = r^.atlas
            NormGMetrics _ advp <- fmap normalizeGlyphMetrics $ glyphMetrics ff ndx
            let w'  = fromIntegral w 
                adv = realToFrac advp * w' 
            return (x + adv, y)

drawCharacter r (x,y) char =
    let mChar = IM.lookup (fromEnum char) $ r^.atlas.atlasMap
    in
    case mChar of
        Nothing -> return (x,y)
        Just fc@(FontChar tex (w,h) ndx) -> do
            let p  = r^.textProgram.program
                Atlas _ ff pxS _ = r^.atlas

            -- Get the metrics about the char.
            NormGMetrics (bXp,bYp) advp <- fmap normalizeGlyphMetrics $ glyphMetrics ff ndx

            let sW = fromIntegral w
                sH = fromIntegral h
                x' = x + sW * realToFrac bXp
                y' = (y + fromIntegral pxS) - sH * realToFrac bYp
                a  = realToFrac advp * sW
                txy = translationMatrix3d x' y' 0
                sxy = scaleMatrix3d sW sH 1 :: Matrix GLfloat
                mv  = identityN 4 :: Matrix GLfloat
                mv' = mv `multiply` txy `multiply` sxy

            texture Texture2D $= Enabled
            activeTexture $= TextureUnit 0
            textureBinding Texture2D $= Just tex
            r^.textProgram.setModelview $ concat mv'
            r^.quadUVRender
            return (x + a, y)


glyphMetrics :: FT_Face -> CUInt -> IO FT_Glyph_Metrics
glyphMetrics ff ndx = do
    runFreeType $ ft_Load_Glyph ff ndx 0
    slot <- peek $ glyph ff
    peek $ GS.metrics slot


normalizeGlyphMetrics :: FT_Glyph_Metrics -> NormalizedGlyphMetrics
normalizeGlyphMetrics m = NormGMetrics bxy adv
    where bX  = (fromIntegral $ horiBearingX m) % fromIntegral (GM.width m)
          bY  = (fromIntegral $ horiBearingY m) % fromIntegral (GM.height m)
          bxy = (bX,bY)
          adv = fromIntegral (horiAdvance m) % fromIntegral (GM.width m) 


initTextRenderer :: FilePath -> Int -> IO TextRenderer
initTextRenderer font px = do
    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    depthFunc $= Nothing

    putStrLn "Text shader."
    v <- makeShader VertexShader vertSrc
    f <- makeShader FragmentShader fragSrc

    p <- makeProgram [v,f] [("position", AttribLocation 0), ("uv", AttribLocation 1)]
    currentProgram $= Just p
    printError

    UniformLocation mv <- get $ uniformLocation p "modelview"
    UniformLocation pj <- get $ uniformLocation p "projection"

    let updateMV mat = withArray mat $ \ptr ->
                           glUniformMatrix4fv mv 1 1 ptr
        updatePJ mat = withArray mat $ \ptr ->
                           glUniformMatrix4fv pj 1 1 ptr

    sLoc <- get $ uniformLocation p "sampler"
    cLoc <- get $ uniformLocation p "color"
    let updateSampler s = uniform sLoc $= s
        updateColor c   = uniform cLoc $= c



    let verts = [ 0, 0
                , 1, 0
                , 1, 1
                , 0, 0
                , 1, 1
                , 0, 1
                ] :: [GLfloat]
        uvs    = verts
        size   = length verts * sizeOf (undefined :: Float)
    [i,j] <- genObjectNames 2

    -- Buffer the verts
    bindVBO i vertDescriptor $ AttribLocation 0
    withArray verts $ \ptr ->
        bufferData ArrayBuffer $= (fromIntegral size, ptr, StaticDraw)

    -- Buffer the uvs
    bindVBO j uvDescriptor $ AttribLocation 1
    withArray uvs $ \ptr ->
        bufferData ArrayBuffer $= (fromIntegral size, ptr, StaticDraw)


    let quadUV = do bindVBO i vertDescriptor $ AttribLocation 0
                    bindVBO j uvDescriptor $ AttribLocation 1
                    drawArrays Triangles 0 6
                    bindBuffer ArrayBuffer $= Nothing

    putStrLn $ "Initializing atlas at size " ++ show px
    a <- initAtlas font px


    return TextRenderer { _textProgram    = RndrProgram3D { _program = p
                                                          , _setModelview  = updateMV
                                                          , _setProjection = updatePJ
                                                          }
                        , _setSampler = updateSampler
                        , _setTextColor = updateColor
                        , _atlas = a
                        , _quadUVRender = quadUV
                        }

