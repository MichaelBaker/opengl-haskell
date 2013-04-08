module Resources where

import Graphics.Rendering.OpenGL.Raw
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Ptr
import Foreign.C.String

data Resources = Resources { verticies      :: VertexArray
                           , elements       :: ElementArray
                           , vertexShader   :: GLuint
                           , fragmentShader :: GLuint
                           , program        :: GLuint
                           }

data ElementArray = ElementArray { elementBuffer :: GLuint
                                 , triangleType  :: GLenum
                                 , vertexCount   :: GLsizei
                                 , indexType     :: GLenum
                                 }

data VertexArray = VertexArray { vertexBuffer   :: GLuint
                               , attribute      :: GLuint
                               , itemsPerVertex :: GLint
                               , itemType       :: GLenum
                               , vertexSize     :: GLsizei
                               }

enableAttributePointer vertexArray = do
  glBindBuffer gl_ARRAY_BUFFER (vertexBuffer vertexArray)
  glVertexAttribPointer
    (attribute      vertexArray)
    (itemsPerVertex vertexArray)
    (itemType       vertexArray)
    0
    (vertexSize     vertexArray)
    nullPtr
  glEnableVertexAttribArray (attribute vertexArray)

disableAttributePointer vertexArray = glDisableVertexAttribArray $ attribute vertexArray

drawElements elementArray = do
  glBindBuffer gl_ELEMENT_ARRAY_BUFFER (elementBuffer elementArray)
  glDrawElements
    (triangleType elementArray)
    (vertexCount  elementArray)
    (indexType    elementArray)
    nullPtr

vertexList :: [GLfloat]
vertexList = [ -1.0, -1.0, 0.0, 1.0
             ,  1.0, -1.0, 0.0, 1.0
             , -1.0,  1.0, 0.0, 1.0
             ,  1.0,  1.0, 0.0, 1.0
             ]

elementList :: [GLshort]
elementList = [0, 1, 2, 1, 2, 3]

createResources = do
  vertexArrayPtr  <- newArray vertexList
  elementArrayPtr <- newArray elementList
  vBuffer  <- createBuffer gl_ARRAY_BUFFER         vertexArrayPtr  (listSize vertexList)
  eBuffer  <- createBuffer gl_ELEMENT_ARRAY_BUFFER elementArrayPtr (listSize elementList)
  vShader  <- createShader gl_VERTEX_SHADER   "gl.v.glsl"
  fShader  <- createShader gl_FRAGMENT_SHADER "gl.f.glsl"
  program  <- createProgram vShader fShader
  position <- withCString "position" $ \str -> glGetAttribLocation program str

  let elementArray = ElementArray eBuffer gl_TRIANGLES 6 gl_UNSIGNED_SHORT
      vertexArray  = VertexArray  vBuffer (fromIntegral position) 4 gl_FLOAT (fromIntegral $ 4 * sizeOf (0 :: GLfloat))

  return $ Resources vertexArray elementArray vShader fShader program

createProgram vShader fShader = do
  alloca $ \programOk -> do
    program <- glCreateProgram
    glAttachShader program vShader
    glAttachShader program fShader
    glLinkProgram program
    glGetProgramiv program gl_LINK_STATUS programOk
    programOkVal <- peek programOk
    if programOkVal == 0
      then error "Program failed to compile"
      else return program

createShader shaderType filename = do
  sourceCode <- readFile filename
  shader     <- glCreateShader shaderType
  withCString sourceCode $ \source -> do
    alloca $ \shaderOk -> do
      alloca $ \sourceList -> do
        alloca $ \len -> do
          poke sourceList source
          poke len $ fromIntegral $ length sourceCode
          glShaderSource shader 1 sourceList len
          glCompileShader shader
          glGetShaderiv shader gl_COMPILE_STATUS shaderOk
          shaderOkVal <- peek shaderOk
          if shaderOkVal == 0
            then error $ "Shader " ++ filename ++ " failed to compile"
            else return shader

listSize list = fromIntegral $ sizeOf (head list) * length list

createBuffer target bufferData size = do
  bufferPtr <- (malloc :: IO (Ptr GLuint))
  glGenBuffers 1 bufferPtr

  buffer <- peek bufferPtr
  glBindBuffer target buffer
  glBufferData target size bufferData gl_STATIC_DRAW

  return buffer
