module Graphics.Rendering.Shader.Types where

import Graphics.Rendering.OpenGL


-- | A function that updates a 4x4 matrix uniform.
type SetUniformMatrix4fv = [GLfloat] -> IO ()

-- | A function that updates an int uniform.
type SetUniform1i = Index1 GLint -> IO () 

-- | A function that updates a bool uniform.
type SetUniformBool = Bool -> IO ()

-- | A function that updates a color uniform.
type SetUniformColor4f = Color4 GLfloat -> IO ()

