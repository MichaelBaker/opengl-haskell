module Cube where

import Foreign
import Graphics.Rendering.OpenGL.Raw

import Job
import Resources
import Renderable

type Vertex = (GLfloat, GLfloat, GLfloat, GLfloat)

type Color = (GLfloat, GLfloat, GLfloat, GLfloat)

data Face = Face { vertex0 :: Vertex
                 , vertex1 :: Vertex
                 , vertex2 :: Vertex
                 , vertex3 :: Vertex
                 , color   :: Color
                 }

data Cube = Cube { position :: Vertex
                 , face0    :: Face
                 , face1    :: Face
                 , face2    :: Face
                 , face3    :: Face
                 , face4    :: Face
                 , face5    :: Face
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
  program    <- createCubeProgram shader
  attributes <- createCubeAttributes program (cube x z)
  elements   <- createCubeElements
  vfov       <- createUniform program "vfov"
  hfov       <- createUniform program "hfov"
  sunAngle   <- createUniform program "sunAngle"
  return $ CubeJob (Job program attributes elements) vfov (pi/2.0) hfov (pi/2.0) sunAngle (pi/2.0)

cube x z = theCube { position = (x, 0.0, z, 1.0) }

createCubeProgram shader = do
  vertexShader   <- createShader gl_VERTEX_SHADER   $ concat ["../../shaders/", shader, ".v.glsl"]
  fragmentShader <- createShader gl_FRAGMENT_SHADER $ concat ["../../shaders/", shader, ".f.glsl"]
  program        <- createProgram [vertexShader, fragmentShader]
  return program

createCubeAttributes program cube = do
  position    <- attributeId program "position"
  faceColor   <- attributeId program "faceColor"
  translation <- attributeId program "translation"
  normal      <- attributeId program "normal"

  let vertexArray = attributeArray cube
  vertexArrayPtr <- newArray vertexArray
  vertexBuffer   <- createBuffer gl_ARRAY_BUFFER vertexArrayPtr (listSize vertexArray)

  let positions    = AttributeArray vertexBuffer position    4 gl_FLOAT (fromIntegral $ 16 * floatSize) nullPtr
      faceColors   = AttributeArray vertexBuffer faceColor   4 gl_FLOAT (fromIntegral $ 16 * floatSize) (createOffset floatSize 4)
      translations = AttributeArray vertexBuffer translation 4 gl_FLOAT (fromIntegral $ 16 * floatSize) (createOffset floatSize 8)
      normals      = AttributeArray vertexBuffer normal      4 gl_FLOAT (fromIntegral $ 16 * floatSize) (createOffset floatSize 12)

  return [positions, faceColors, translations, normals]

createCubeElements = do
  elementArrayPtr <- newArray elementArray
  elementBuffer   <- createBuffer gl_ELEMENT_ARRAY_BUFFER elementArrayPtr (listSize elementArray)
  return $ ElementArray elementBuffer gl_TRIANGLES (fromIntegral $ length elementArray) gl_UNSIGNED_SHORT

createUniform = uniformId

floatSize = sizeOf (0 :: GLfloat)
createOffset typeSize amount = plusPtr nullPtr $ typeSize * amount

printArray [] = return ()
printArray array = do
  print $ take 4 array
  printArray $ drop 4 array

attributeArray :: Cube -> [GLfloat]
attributeArray cube = concat arrays
  where arrays = map (cubeAttributes $ position cube) (faces cube)

cubeAttributes position face = triangle0 ++ triangle1
  where triangle0            = concat [listify vertex2 vertex1 vertex0]
        triangle1            = concat [listify vertex0 vertex3 vertex2]
        normal               = cross (minus (vertex2 face) (vertex1 face))
                                     (minus (vertex0 face) (vertex1 face))
        detuple (a, b, c, d) = [a, b, c, d]
        listify a b c        = concat $ map detuple [a face, color face, position, normal,
                                                     b face, color face, position, normal,
                                                     c face, color face, position, normal]

cross (a, b, c, d) (x, y, z, w) = (b*z - c*y, c*x - a*z, a*y - b*x, 1.0)
minus (a, b, c, d) (x, y, z, w) = (a - x, b - y, c - z, 1.0)

faces cube = [ face0 cube
             , face1 cube
             , face2 cube
             , face3 cube
             , face4 cube
             , face5 cube
             ]

listSize list = fromIntegral $ sizeOf (head list) * length list

elementArray :: [GLshort]
elementArray = [0..35]
