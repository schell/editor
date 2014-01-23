{-# LANGUAGE TemplateHaskell #-}
module Graphics.Cacheing.Types where

import Graphics.Rendering.OpenGL


data RenderCache = RenderCache { _cacheTexture :: TextureObject
                               , _cachePos     :: Position
                               , _cacheSize    :: Size
                               }

