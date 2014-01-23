{-# LANGUAGE TemplateHaskell #-}
module Graphics.Rendering.Text.Types where

import           Graphics.Rendering.Shader.Text.Types
import           Graphics.Rendering.OpenGL
import           Control.Lens
import qualified Data.IntMap as IM


type PenPosition = Position


type BufferGeom = ([GLfloat], [GLfloat])


data NormalizedGlyphMetrics = NormGMetrics { _ngmBearing :: (Rational, Rational)
                                           , _ngmAdvance :: Rational
                                           } deriving (Show, Eq)


data FontChar = FontChar { _fcTextureSize   :: Size
                         , _fcTextureOffset :: Position
                         , _fcNormMetrics   :: NormalizedGlyphMetrics
                         } deriving (Show, Eq)
makeLenses ''FontChar


data Atlas = Atlas { _atlasFontFilePath  :: FilePath
                   , _atlasTextureObject :: TextureObject
                   , _atlasTextureSize   :: Size 
                   , _atlasPxSize        :: GLsizei
                   , _atlasMap           :: IM.IntMap FontChar
                   }
makeLenses ''Atlas


data BufferAccumulator = BufferAcc { _buffAccAtlas  :: Atlas
                                   , _buffAccGeom   :: BufferGeom
                                   , _buffAccPos    :: Position
                                   , _buffAccSize   :: Size
                                   }
makeLenses ''BufferAccumulator


data TextRenderer = TextRenderer { _shader :: TextShaderProgram
                                 , _atlas  :: Atlas
                                 }
makeLenses ''TextRenderer


