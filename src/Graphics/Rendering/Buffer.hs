module Graphics.Rendering.Buffer where

import           Editor.Buffer.Types
import           Graphics.Text.Renderer


-- | Renders a buffer into its BufferRenderCache
renderBuffer :: Buffer -> TextRenderer -> Buffer
renderBuffer buffer renderer = do
    let (BufferAcc _ (vs,uvs) _) 
    
