module Sphere where

import Graphics.Rendering.OpenGL.Raw

import Job
import Resources
import Renderable

data Sphere = Sphere { radius   :: GLfloat
                     , color    :: Vertex
                     , position :: Vertex
                     }

data SphereJob = SphereJob { job :: Job }

data Triangle = Triangle Vertex Vertex Vertex

instance Renderable SphereJob where
  render (SphereJob job) = render job

createSphere (x, z) = do
  program    <- createProgram "sphere"
  attributes <- createGenericAttributes program $ sphereAttributes x z
  elements   <- createElements [0..(fromIntegral $ length icosahedron * 3 - 1)]
  return $ SphereJob $ Job program attributes elements

phi = (1.0 + sqrt 5.0) / 2.0

icosahedron = map makeTriangle indicies
  where makeTriangle (a, b, c) = Triangle (points !! a) (points !! b) (points !! c)
        points = [ (-1.0, phi, 0.0, 1.0)
                 , ( 1.0, phi, 0.0, 1.0)
                 , (-1.0,-phi, 0.0, 1.0)
                 , ( 1.0,-phi, 0.0, 1.0)
                 , ( 0.0,-1.0, phi, 1.0)
                 , ( 0.0, 1.0, phi, 1.0)
                 , ( 0.0,-1.0,-phi, 1.0)
                 , ( 0.0, 1.0,-phi, 1.0)
                 , ( phi, 0.0,-1.0, 1.0)
                 , ( phi, 0.0, 1.0, 1.0)
                 , (-phi, 0.0,-1.0, 1.0)
                 , (-phi, 0.0, 1.0, 1.0)
                 ]
        indicies = [ ( 0, 11,  5)
                   , ( 0,  5,  1)
                   , ( 0,  1,  7)
                   , ( 0,  7, 10)
                   , ( 0, 10, 11)
                   , ( 1,  5,  9)
                   , ( 5, 11,  4)
                   , (11, 10,  2)
                   , (10,  7,  6)
                   , ( 7,  1,  8)
                   , ( 3,  9,  4)
                   , ( 3,  4,  2)
                   , ( 3,  2,  6)
                   , ( 3,  6,  8)
                   , ( 3,  8,  9)
                   , ( 4,  9,  5)
                   , ( 2,  4, 11)
                   , ( 6,  2, 10)
                   , ( 8,  6,  7)
                   , ( 9,  8,  1)
                   ]

triangleArray x z ((Triangle a b c), color) = concat $ map toList [a, b, c]
  where toList t = concat [ normalize t
                          , [color, color, color, 1.0]
                          , [  x, 0.0,   z, 1.0]
                          , normalize t
                          ]
        normalize (x, y, z, w) = [x/l, y/l, z/l, w]
          where l = sqrt $ (x^2) + (y^2) + (z^2)

sphereAttributes x z = concat $ map (triangleArray x z) (zip icosahedron [0.01,0.04..])
