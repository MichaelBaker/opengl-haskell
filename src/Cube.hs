module Cube where

import Foreign
import Graphics.Rendering.OpenGL.Raw

import Job
import Resources

type Vertex = (GLfloat, GLfloat, GLfloat, GLfloat)

type Color = (GLfloat, GLfloat, GLfloat, GLfloat)

data Face = Face { vertex0 :: Vertex
                 , vertex1 :: Vertex
                 , vertex2 :: Vertex
                 , vertex3 :: Vertex
                 , color   :: Color
                 }

data Cube = Cube { face0 :: Face
                 , face1 :: Face
                 , face2 :: Face
                 , face3 :: Face
                 , face4 :: Face
                 , face5 :: Face
                 }

theCube = Cube
  (Face (-0.5,  0.5,  0.5, 1.0 )
        ( 0.5,  0.5,  0.5, 1.0 )
        ( 0.5, -0.5,  0.5, 1.0 )
        (-0.5, -0.5,  0.5, 1.0 )
        ( 0.0,  0.0,  0.0, 1.0 ))

  (Face ( 0.5,  0.5,  0.5, 1.0 )
        ( 0.5,  0.5, -0.5, 1.0 )
        ( 0.5, -0.5, -0.5, 1.0 )
        ( 0.5, -0.5,  0.5, 1.0 )
        ( 0.1,  0.1,  0.1, 1.0 ))

  (Face (-0.5,  0.5, -0.5, 1.0 )
        ( 0.5,  0.5, -0.5, 1.0 )
        ( 0.5, -0.5, -0.5, 1.0 )
        (-0.5, -0.5, -0.5, 1.0 )
        ( 0.2,  0.2,  0.2, 1.0 ))

  (Face (-0.5,  0.5,  0.5, 1.0 )
        (-0.5,  0.5, -0.5, 1.0 )
        (-0.5, -0.5, -0.5, 1.0 )
        (-0.5, -0.5,  0.5, 1.0 )
        ( 0.3,  0.3,  0.3, 1.0 ))

  (Face (-0.5,  0.5,  0.5, 1.0 )
        (-0.5,  0.5, -0.5, 1.0 )
        ( 0.5,  0.5, -0.5, 1.0 )
        ( 0.5,  0.5,  0.5, 1.0 )
        ( 0.4,  0.4,  0.4, 1.0 ))

  (Face (-0.5, -0.5,  0.5, 1.0 )
        (-0.5, -0.5, -0.5, 1.0 )
        ( 0.5, -0.5, -0.5, 1.0 )
        ( 0.5, -0.5,  0.5, 1.0 )
        ( 0.5,  0.5,  0.5, 1.0 ))

createCube = do
  program    <- createCubeProgram
  attributes <- createCubeAttributes program
  elements   <- createCubeElements
  return $ Job program attributes elements

createCubeProgram = do
  vertexShader   <- createShader gl_VERTEX_SHADER   "../../shaders/gl.v.glsl"
  fragmentShader <- createShader gl_FRAGMENT_SHADER "../../shaders/gl.f.glsl"
  program        <- createProgram [vertexShader, fragmentShader]
  return program

createCubeAttributes program = do
  position  <- attributeId program "position"
  faceColor <- attributeId program "faceColor"

  let vertexArray = attributeArray theCube
  vertexArrayPtr <- newArray vertexArray
  vertexBuffer   <- createBuffer gl_ARRAY_BUFFER vertexArrayPtr (listSize vertexArray)

  let positions  = AttributeArray vertexBuffer position  4 gl_FLOAT (fromIntegral $ 8 * floatSize) nullPtr
      faceColors = AttributeArray vertexBuffer faceColor 4 gl_FLOAT (fromIntegral $ 8 * floatSize) (createOffset floatSize 4)

  return [positions, faceColors]

floatSize = sizeOf (0 :: GLfloat)
createOffset typeSize amount = plusPtr nullPtr $ typeSize * amount

printArray [] = return ()
printArray array = do
  print $ take 4 array
  printArray $ drop 4 array

createCubeElements = do
  elementArrayPtr <- newArray elementArray
  elementBuffer   <- createBuffer gl_ELEMENT_ARRAY_BUFFER elementArrayPtr (listSize elementArray)
  return $ ElementArray elementBuffer gl_TRIANGLES (fromIntegral $ length elementArray) gl_UNSIGNED_SHORT

attributeArray :: Cube -> [GLfloat]
attributeArray cube = concat arrays
  where arrays = map cubeAttributes (faces cube)

cubeAttributes face = triangle0 ++ triangle1
  where triangle0            = concat [listify vertex0 vertex1 vertex2]
        triangle1            = concat [listify vertex2 vertex3 vertex0]
        listify a b c        = concat $ map detuple [a face, color face, b face, color face, c face, color face]
        detuple (a, b, c, d) = [a, b, c, d]

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
