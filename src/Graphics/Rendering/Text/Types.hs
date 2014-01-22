{-# LANGUAGE TemplateHaskell #-}
module Graphics.Rendering.Text.Types where

import           Graphics.Rendering.Shader.Text.Types
import           Graphics.Rendering.OpenGL
import           Control.Lens
import qualified Data.IntMap as IM


type PenPosition = (GLfloat, GLfloat)


type BufferGeom = ([GLfloat], [GLfloat])


data NormalizedGlyphMetrics = NormGMetrics { _ngmBearing :: (Rational, Rational)
                                           , _ngmAdvance :: Rational
                                           } deriving (Show, Eq)


data FontChar = FontChar { _fcTextureSize   :: (Int, Int)
                         , _fcTextureOffset :: (Int, Int)
                         , _fcNormMetrics   :: NormalizedGlyphMetrics
                         } deriving (Show, Eq)
makeLenses ''FontChar


data Atlas = Atlas { _atlasFontFilePath  :: FilePath
                   , _atlasTextureObject :: TextureObject
                   , _atlasTextureSize   :: (Int, Int)
                   , _atlasPxSize        :: Int
                   , _atlasMap           :: IM.IntMap FontChar
                   }
makeLenses ''Atlas


data BufferAccumulator = BufferAcc { _buffAccAtlas  :: Atlas
                                   , _buffAccGeom   :: BufferGeom
                                   , _buffAccPenPos :: PenPosition
                                   , _buffAccSize   :: (GLfloat, GLfloat)
                                   }
makeLenses ''BufferAccumulator


data TextRenderer = TextRenderer { _shader :: TextShaderProgram
                                 , _atlas  :: Atlas
                                 }
makeLenses ''TextRenderer


