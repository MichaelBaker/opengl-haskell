module Bezier (strokedBezier) where

import Graphics.Rendering.OpenGL.Raw
import Attribute
import Element
import Job
import Shader
import Uniform

strokedBezier (x0, y0) (x1, y1) (x2, y2) = do
  program    <- createProgram "bezier"
  attributes <- createAttributes program points description
  elements   <- createElements [0, 1, 2, 2, 3, 0]
  control0   <- createUniform program $ Uniform2FloatsDescription "c0" x0 y0
  control1   <- createUniform program $ Uniform2FloatsDescription "c1" x1 y1
  control2   <- createUniform program $ Uniform2FloatsDescription "c2" x2 y2
  return $ Job program attributes elements [control0, control1, control2]
    where points      = [ -1, -1, 0, 1
                        ,  1, -1, 0, 1
                        ,  1,  1, 0, 1
                        , -1,  1, 0, 1
                        ]
          description = [ fourFloatVector { attributeName = "position"} ]
