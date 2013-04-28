module Sphere where

import Graphics.Rendering.OpenGL.Raw

import Job
import Resources
import Renderable
import Uniform

data SphereJob = SphereJob { quality          :: Int
                           , sphereSunAngle   :: Uniform GLfloat
                           , aspectRatio      :: Uniform GLfloat
                           , specular         :: Uniform GLint
                           , shininess        :: Uniform GLfloat
                           , gamma            :: Uniform GLfloat
                           , range            :: Uniform GLfloat
                           , job              :: Job
                           }

data Triangle = Triangle Vertex Vertex Vertex

instance Renderable SphereJob where
  render sphere = do
    render $ sphereSunAngle sphere
    render $ aspectRatio    sphere
    render $ specular       sphere
    render $ shininess      sphere
    render $ gamma          sphere
    render $ range          sphere
    render $ job            sphere

createSphere quality aspectRatio (x, y, z) = do
  program     <- createProgram "sphere"
  attributes  <- createGenericAttributes program $ sphereAttributes quality x y z
  elements    <- createElements [0..(fromIntegral $ 20 * 3 * (4^quality))]
  sunAngle    <- createUniformFloat program "sunAngle"    0.0
  aspectRatio <- createUniformFloat program "aspectRatio" aspectRatio
  shininess   <- createUniformFloat program "shininess"   0.08
  gamma       <- createUniformFloat program "gamma"       0.85
  range       <- createUniformFloat program "range"       1.25
  specular    <- createUniformInt   program "specular"    2
  return $ SphereJob quality sunAngle aspectRatio specular shininess gamma range $ Job program attributes elements

phi = (1.0 + sqrt 5.0) / 2.0

alterShininess amount sphere = sphere { shininess = alterUniform (shininess sphere) (+ amount) }
alterGamma     amount sphere = sphere { gamma     = alterUniform (gamma sphere)     (+ amount) }
alterRange     amount sphere = sphere { range     = alterUniform (range sphere)     (+ amount) }
setSpecular    number sphere = sphere { specular  = setUniform (specular sphere)    number }

clamp max value | value > max = 0
                | otherwise   = value
updateSphereSunAngle sphere = sphere { sphereSunAngle = alterUniform (sphereSunAngle sphere) $ clamp (2.0 * pi) . (+ 0.02) }

icosahedron = map makeTriangle indicies
  where makeTriangle (a, b, c) = Triangle (normalize $ points !! a) (normalize $ points !! b) (normalize $ points !! c)
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

triangleArray x y z (Triangle a b c) = concat $ map toList [a, b, c]
  where toList t = concat [ detuple t
                          , [1.0, 0.0, 0.0, 1.0]
                          , [  x,   y,   z, 1.0]
                          , detuple t
                          ]

refineTriangle (Triangle a b c) = [Triangle a midAB midCA, Triangle b midBC midAB, Triangle c midCA midBC, Triangle midAB midBC midCA]
  where midAB = normalize $ midpoint a b
        midBC = normalize $ midpoint b c
        midCA = normalize $ midpoint c a

refineSphere 0 ts = ts
refineSphere q ts = refineSphere (q - 1) $ concat (map refineTriangle ts)

sphereAttributes quality x y z = concat $ map (triangleArray x y z) (refineSphere quality icosahedron)
