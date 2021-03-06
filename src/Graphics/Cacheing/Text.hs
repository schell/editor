module Graphics.Cacheing.Text (
    module T,
    cacheText
) where

import Control.Lens
import Data.Monoid
import Graphics.Rendering.OpenGL
import Graphics.Cacheing.Types as T
import Graphics.Rendering.Text
import Graphics.Rendering.Shader.Text
import Graphics.Utils
import Graphics.Math


-- | Renders a string of text into a texture and returns a render cache.
cacheText :: TextRenderer -> String -> IO RenderCache
cacheText r s = do
    let (BufferAcc _ (vs,uvs) _ size) = geometryForString (BufferAcc (r^.atlas) mempty (Position 0 0) (Size 0 0)) s
    t <- renderToTexture size RGB8 $ do
        (i,j) <- bindAndBufferVertsUVs vs uvs
        texture Texture2D $= Enabled
        activeTexture $= TextureUnit 0
        textureBinding Texture2D $= Just (r^.atlas.atlasTextureObject)
        r^.shader.setSampler $ Index1 0
        r^.shader.setModelview $ concat $ identityN 4
        drawArrays Triangles 0 $ fromIntegral $ 6 * length s
        bindBuffer ArrayBuffer $= Nothing
        deleteObjectNames [i,j]

    return RenderCache { _cacheTexture = t
                       , _cachePos = Position 0 0
                       , _cacheSize = size
                       }
