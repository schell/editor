{-# LANGUAGE OverloadedStrings #-}
module Graphics.Rendering.Text.Renderer (
    makeTextRenderer,
    drawTextAt,
    drawTextAt',
    loadCharMap,
) where

import           Graphics.Math
import           Graphics.Rendering.Text.Character
import           Graphics.Rendering.Text.Font
import           Graphics.Rendering.Text.Types
import           Graphics.Rendering.Shader.Text
import           Graphics.Rendering.OpenGL hiding (Bitmap, Matrix)
import           Control.Monad
import           Control.Lens
import           Data.Monoid
import qualified Data.IntMap as IM

makeAtlas :: FilePath -> Int -> IO Atlas
makeAtlas fp px = do
    -- Get the missing glyph as a texture and FontChar.
    (tex, fChar) <- texturizeGlyphOfEnum fp px '\NUL'

    -- Store that in our atlas.
    return Atlas { _atlasFontFilePath = fp
                 , _atlasTextureObject = tex
                 , _atlasTextureSize = _fcTextureSize fChar
                 , _atlasPxSize = px
                 , _atlasMap = IM.insert 0 fChar IM.empty
                 }


drawTextAt :: TextRenderer -> PenPosition -> String -> IO ()
drawTextAt r (x,y) = foldM_ foldCharacter (x,y)
    where foldCharacter (_,y') '\n' = return (x, y' + fromIntegral (r^.atlas.atlasPxSize))
          foldCharacter p c          = drawChar r p c


drawTextAt' :: TextRenderer -> PenPosition -> String -> IO ()
drawTextAt' r pen s = do
    let (BufferAcc _ (vs,uvs) _ (w,h)) = geometryForString (BufferAcc (r^.atlas) mempty pen (0,0)) s
    putStrLn $ "Text is " ++ show w ++ "px x " ++ show h ++ "px"
    (i,j) <- bindAndBufferVertsUVs vs uvs
    texture Texture2D $= Enabled
    activeTexture $= TextureUnit 0
    textureBinding Texture2D $= Just (r^.atlas.atlasTextureObject)
    r^.shader.setSampler $ Index1 0
    r^.shader.setModelview $ concat $ identityN 4
    drawArrays Triangles 0 $ fromIntegral $ 6 * length s
    bindBuffer ArrayBuffer $= Nothing
    deleteObjectNames [i,j]


makeTextRenderer :: FilePath -> Int -> IO TextRenderer
makeTextRenderer font px = do
    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    depthFunc $= Nothing

    s <- makeTextShaderProgram
    a <- makeAtlas font px

    return TextRenderer { _shader = s
                        , _atlas = a
                        }

