module Circle where

import Graphics.Rendering.OpenGL.Raw
import Attribute
import Element
import Job
import Shader
import Uniform

createCircle (x, y, z) = do
  program    <- createProgram "circle"
  attributes <- createAttributes program points description
  elements   <- createElements [0, 1, 2, 2, 3, 0]
  center     <- createUniform program $ Uniform3FloatsDescription "center" x y z
  return $ Job program attributes elements [center]
    where points = [-1, -1, 0, 1
                   , 1, -1, 0, 1
                   , 1,  1, 0, 1
                   ,-1,  1, 0, 1
                   ]
          description = [ fourFloatVector { attributeName = "position" } ]
