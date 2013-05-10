module Bezier (quadraticBezier) where

import Graphics.Rendering.OpenGL.Raw
import Attribute
import Element
import Job
import Shader

type Point = (GLfloat, GLfloat, GLfloat)

quadraticBezier :: Point -> Point -> Point -> IO Job
quadraticBezier (x, y, z) (x', y', z') (x'', y'', z'') = do
  program    <- createProgram "bezier"
  attributes <- createAttributes program points description
  elements   <- createElements [0, 1, 2, 2, 3, 0]
  return $ Job program attributes elements []
    where points      = [ -1, -1, 0, 1, 0
                        ,  1, -1, 0, 1, 1
                        ,  1,  1, 0, 1, 2
                        , -1,  1, 0, 1, 3
                        ]
          description = [ fourFloatVector { attributeName = "position"}
                        , singleFloat     { attributeName = "index"}
                        ]
