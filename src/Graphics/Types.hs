{-# LANGUAGE TemplateHaskell #-}
module Graphics.Types where

import Graphics.Rendering.OpenGL
import Control.Lens
import Foreign.C.Types
import Graphics.Rendering.FreeType.Internal.Library
import Graphics.Rendering.FreeType.Internal.Face
import Graphics.Rendering.FreeType.Internal.GlyphMetrics
import Graphics.Rendering.FreeType.Internal.SizeMetrics
import Data.Ratio
import qualified Data.IntMap as IM


data TextResource  = Font FilePath | Bitmap FilePath


data FontChar = FontChar { _fcTextureObject :: TextureObject
                         , _fcTextureSize   :: (Int,Int)
                         , _fcGlyphIndex    :: CUInt
                         } deriving (Show, Eq)
makeLenses ''FontChar


data NormalizedGlyphMetrics = NormGMetrics { _ngmBearing :: (Rational, Rational)
                                           , _ngmAdvance :: Rational
                                           }


data Atlas = Atlas { _atlasFreeType :: FT_Library
                   , _atlasFontFace :: FT_Face
                   , _atlasPxSize   :: Int
                   , _atlasMap      :: IM.IntMap FontChar
                   }
makeLenses ''Atlas


type Rendering = IO (IO ())


type MatrixUpdate = [GLfloat] -> IO ()


data RndrProgram3D = RndrProgram3D { _program :: Program
                                   , _setProjection :: MatrixUpdate
                                   , _setModelview  :: MatrixUpdate
                                   }
makeLenses ''RndrProgram3D


data TextRenderer = TextRenderer { _textProgram  :: RndrProgram3D
                                 , _setSampler   :: Index1 GLint -> IO ()
                                 , _setTextColor :: Color4 GLfloat -> IO ()
                                 , _atlas        :: Atlas
                                 , _quadUVRender :: IO ()
                                 }
makeLenses ''TextRenderer


data QuadRenderer = QuadRenderer { _quadProgram  :: RndrProgram3D
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


