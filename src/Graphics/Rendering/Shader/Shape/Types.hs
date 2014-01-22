{-# LANGUAGE TemplateHaskell #-}
module Graphics.Rendering.Shader.Shape.Types where

import Graphics.Rendering.OpenGL
import Control.Lens
import Graphics.Rendering.Shader.Types


data ShapeShaderProgram = ShapeShaderProgram { _program       :: Program
                                             , _setProjection :: SetUniformMatrix4fv
                                             , _setModelview  :: SetUniformMatrix4fv
                                             , _setSampler    :: SetUniform1i
                                             , _setIsTextured :: SetUniformBool
                                             }
makeLenses ''ShapeShaderProgram

