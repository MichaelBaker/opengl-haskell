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

phi = (1.0 + sqrt 5.0) / 2.0

instance Renderable SphereJob where
  render (SphereJob job) = render job

createSphere = do
  program    <- createProgram "perspective-2"
  attributes <- createGenericAttributes program sphereAttributes
  elements   <- createElements [0..2]
  return $ SphereJob $ Job program attributes elements

sphereAttributes = [0.0, 0.0, 0.0, 1.0,
                    0.0, 0.0, 0.5, 1.0,
                    0.0, 0.0, 3.0, 0.0,
                    1.0, 0.0, 0.0, 0.0,

                    10.0, 10.0, 0.0, 1.0,
                    0.0, 0.0, 0.5, 1.0,
                    0.0, 0.0, 3.0, 0.0,
                    1.0, 0.0, 0.0, 0.0,

                    10.0, -10.0, 0.0, 1.0,
                    0.0, 0.0, 0.5, 1.0,
                    0.0, 0.0, 3.0, 0.0,
                    1.0, 0.0, 0.0, 0.0
                    ]
