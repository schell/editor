{-# LANGUAGE TemplateHaskell #-}
module Graphics.Rendering.Shader.Text.Types where

import Graphics.Rendering.OpenGL
import Control.Lens
import Graphics.Rendering.Shader.Types


data TextShaderProgram = TextShaderProgram { _program       :: Program
                                           , _setProjection :: SetUniformMatrix4fv
                                           , _setModelview  :: SetUniformMatrix4fv
                                           , _setSampler    :: SetUniform1i
                                           , _setTextColor  :: SetUniformColor4f
                                           }
makeLenses ''TextShaderProgram

