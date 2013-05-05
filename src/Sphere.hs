module Sphere where

import Graphics.Rendering.OpenGL.Raw

import Job
import Resources
import Renderable
import Uniform
import Texture

data SphereJob = SphereJob { quality     :: Int
                           , aspectRatio :: Uniform GLfloat
                           , texture     :: Uniform GLint
                           , textureId   :: GLuint
                           , job         :: Job
                           } deriving (Show)

data Triangle = Triangle Vertex Vertex Vertex

instance Renderable SphereJob where
  render sphere = do
    glActiveTexture gl_TEXTURE0
    glBindTexture gl_TEXTURE_2D $ textureId sphere
    render $ aspectRatio    sphere
    render $ texture        sphere
    render $ job            sphere

createSphere quality aspectRatio (x, y, z) = do
  program     <- createProgram "sphere"
  attributes  <- createGenericAttributes program $ sphereAttributes quality x y z
  elements    <- createElements [0..(fromIntegral $ 20 * 3 * (4^quality))]
  aspectRatio <- createUniformFloat program "aspectRatio" aspectRatio
  textureId   <- textureFromImage "josh-cheek" 192 192
  texture     <- createUniformInt program "gradient" 0
  return $ SphereJob quality aspectRatio texture textureId $ Job program attributes elements

phi = (1.0 + sqrt 5.0) / 2.0

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
