module Bezier (strokedBezier, job) where

import Graphics.Rendering.OpenGL.Raw
import Attribute
import Element
import Job
import Uniform
import Renderable

data BezierJob = BezierJob { job     :: Job
                           , texture :: GLuint
                           }

strokedBezier (x0, y0) (x1, y1) (x2, y2) program image = do
  attributes <- createAttributes program points description
  elements   <- createElements [0, 1, 2, 2, 3, 0]
  control0   <- createUniform program $ Uniform2FloatsDescription "c0" x0 y0
  control1   <- createUniform program $ Uniform2FloatsDescription "c1" x1 y1
  control2   <- createUniform program $ Uniform2FloatsDescription "c2" x2 y2
  texture    <- createUniform program $ UniformIntDescription "image" 0
  return $ BezierJob (Job program attributes elements [control0, control1, control2, texture]) image
    where points      = [ -1, -1, 0, 1, 0
                        ,  1, -1, 0, 1, 1
                        ,  1,  1, 0, 1, 2
                        , -1,  1, 0, 1, 3
                        ]
          description = [ fourFloatVector { attributeName = "position"}
                        , singleFloat     { attributeName = "index" }
                        ]

instance Renderable BezierJob where
  render bezier = do
    glActiveTexture gl_TEXTURE0
    glBindTexture gl_TEXTURE_2D $ texture bezier
    render $ job bezier
