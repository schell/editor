{-# LANGUAGE TemplateHaskell #-}
module Graphics.Types where

import Graphics.Rendering.OpenGL
import Control.Lens
import qualified Data.IntMap as IM
import qualified Data.Map as M


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
                                   }
makeLenses ''BufferAccumulator


-- | Keys an atlas by the px size of the rendered glyphs it holds.
type FontMap = IM.IntMap Atlas


-- | Keys a `FontMap` by the name of the font.
type FontCache = M.Map String FontMap


type Rendering = IO (IO ())


type MatrixUpdate = [GLfloat] -> IO ()


data ShaderProgram = ShaderProgram { _program :: Program
                                   , _setProjection :: MatrixUpdate
                                   , _setModelview  :: MatrixUpdate
                                   }
makeLenses ''ShaderProgram


data TextShaderProgram = TextShaderProgram { _tShader        :: ShaderProgram
                                           , _setSampler     :: Index1 GLint -> IO ()
                                           , _setTextColor   :: Color4 GLfloat -> IO ()
                                           , _renderTexQuad :: IO ()
                                           }
makeLenses ''TextShaderProgram


data TextRenderer = TextRenderer { _textProgram  :: TextShaderProgram
                                 , _atlas        :: Atlas
                                 }
makeLenses ''TextRenderer


data QuadRenderer = QuadRenderer { _qShader      :: ShaderProgram
                                 , _rndrQuad     :: IO ()
                                 , _setQuadColor :: Color4 GLfloat -> IO ()
                                 }
makeLenses ''QuadRenderer


data Renderer = Renderer { _quadRndr   :: QuadRenderer
                         , _textRndr   :: TextRenderer
                         , _screenSize :: (GLfloat, GLfloat)
                         }
makeLenses ''Renderer


instance Show Renderer where
    show _ = "Renderer"


