{-# LANGUAGE TemplateHaskell #-}
module Editor.Buffer.Types where

import Graphics.Rendering.OpenGL
import Graphics.Types
import Control.Lens


data BufferRenderCache = BufferRenderCache { _brcTextureObject :: TextureObject
                                           , _brcPenPos        :: PenPosition
                                           , _brcTextureSize   :: (GLfloat, GLfloat)
                                           }
makeLenses ''BufferRenderCache


data Buffer = Buffer { _bufferContents :: String
                     , _bufferRenderCache :: Maybe BufferRenderCache
                     }
makeLenses ''Buffer


