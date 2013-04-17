module Cube where

import Graphics.Rendering.OpenGL.Raw

import Job
import Resources
import Renderable

data Cube = Cube { position :: Vertex
                 , face0    :: Face
                 , face1    :: Face
                 , face2    :: Face
                 , face3    :: Face
                 , face4    :: Face
                 , face5    :: Face
                 }

data Face = Face { vertex0 :: Vertex
                 , vertex1 :: Vertex
                 , vertex2 :: Vertex
                 , vertex3 :: Vertex
                 , color   :: Color
                 }

data CubeJob = CubeJob { job        :: Job
                       , vfovId     :: GLint
                       , vfov       :: GLfloat
                       , hfovId     :: GLint
                       , hfov       :: GLfloat
                       , sunAngleId :: GLint
                       , sunAngle   :: GLfloat
                       }

instance Renderable CubeJob where
  render (CubeJob job vfovId vfov hfovId hfov sunAngleId sunAngle) = do
    glUniform1f vfovId     vfov
    glUniform1f hfovId     hfov
    glUniform1f sunAngleId sunAngle
    render job

theCube = Cube
  (0.0, 0.0, 3.0, 1.0)

  (Face (-1.0,  1.0, -1.0, 1.0 )
        ( 1.0,  1.0, -1.0, 1.0 )
        ( 1.0, -1.0, -1.0, 1.0 )
        (-1.0, -1.0, -1.0, 1.0 )
        ( 0.5,  0.5,  0.5, 1.0 ))

  (Face ( 1.0,  1.0, -1.0, 1.0 )
        ( 1.0,  1.0,  1.0, 1.0 )
        ( 1.0, -1.0,  1.0, 1.0 )
        ( 1.0, -1.0, -1.0, 1.0 )
        ( 1.0,  0.0,  0.0, 1.0 ))

  (Face (-1.0, -1.0, -1.0, 1.0 )
        (-1.0, -1.0,  1.0, 1.0 )
        (-1.0,  1.0,  1.0, 1.0 )
        (-1.0,  1.0, -1.0, 1.0 )
        ( 1.0,  1.0,  1.0, 1.0 ))

  (Face (-1.0,  1.0,  1.0, 1.0 )
        ( 1.0,  1.0,  1.0, 1.0 )
        ( 1.0, -1.0,  1.0, 1.0 )
        (-1.0, -1.0,  1.0, 1.0 )
        ( 0.5,  0.5,  0.5, 1.0 ))

  (Face (-1.0,  1.0, -1.0, 1.0 )
        (-1.0,  1.0,  1.0, 1.0 )
        ( 1.0,  1.0,  1.0, 1.0 )
        ( 1.0,  1.0, -1.0, 1.0 )
        ( 0.5,  0.5,  0.5, 1.0 ))

  (Face (-1.0, -1.0, -1.0, 1.0 )
        (-1.0, -1.0,  1.0, 1.0 )
        ( 1.0, -1.0,  1.0, 1.0 )
        ( 1.0, -1.0, -1.0, 1.0 )
        ( 0.5,  0.5,  0.5, 1.0 ))

createCube shader (x, z) = do
  program    <- createProgram shader
  attributes <- createGenericAttributes program $ attributeArray (cube x z)
  elements   <- createElements elementArray
  vfov       <- createUniform program "vfov"
  hfov       <- createUniform program "hfov"
  sunAngle   <- createUniform program "sunAngle"
  return $ CubeJob (Job program attributes elements) vfov (pi/2.0) hfov (pi/2.0) sunAngle (pi/2.0)

cube x z = theCube { position = (x, 0.0, z, 1.0) }

modifyhfov cube f = cube { hfov = f $ hfov cube }
modifyvfov cube f = cube { vfov = f $ vfov cube }

attributeArray :: Cube -> [GLfloat]
attributeArray cube = concat arrays
  where arrays = map (cubeAttributes $ position cube) (faces cube)

cubeAttributes position face = triangle0 ++ triangle1
  where triangle0            = concat [listify vertex2 vertex1 vertex0]
        triangle1            = concat [listify vertex0 vertex3 vertex2]
        normal               = cross (minus (vertex2 face) (vertex1 face))
                                     (minus (vertex0 face) (vertex1 face))
        listify a b c        = flattenVerticies [a face, color face, position, normal,
                                                 b face, color face, position, normal,
                                                 c face, color face, position, normal]

faces cube = [ face0 cube
             , face1 cube
             , face2 cube
             , face3 cube
             , face4 cube
             , face5 cube
             ]

elementArray :: [GLshort]
elementArray = [0..35]
