module Resources where

import Graphics.Rendering.OpenGL.Raw
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr
import Foreign.C.String

data ElementArray = ElementArray { elementBuffer :: GLuint
                                 , triangleType  :: GLenum
                                 , vertexCount   :: GLsizei
                                 , indexType     :: GLenum
                                 }

data AttributeArray = AttributeArray { attributeBuffer :: GLuint
                                     , attribute       :: GLuint
                                     , itemsPerVertex  :: GLint
                                     , itemType        :: GLenum
                                     , vertexSize      :: GLsizei
                                     , offset          :: Ptr GLuint
                                     }

enableAttribute attributeArray = do
  glBindBuffer gl_ARRAY_BUFFER (attributeBuffer attributeArray)
  glVertexAttribPointer
    (attribute      attributeArray)
    (itemsPerVertex attributeArray)
    (itemType       attributeArray)
    0
    (vertexSize     attributeArray)
    (offset         attributeArray)
  glEnableVertexAttribArray (attribute attributeArray)

disableAttribute vertexArray = glDisableVertexAttribArray $ attribute vertexArray

drawElements elementArray = do
  glBindBuffer gl_ELEMENT_ARRAY_BUFFER (elementBuffer elementArray)
  glDrawElements
    (triangleType elementArray)
    (vertexCount  elementArray)
    (indexType    elementArray)
    nullPtr

createProgram shaders = do
  alloca $ \programOk -> do
    program <- glCreateProgram
    mapM_ (glAttachShader program) shaders
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

createBuffer target bufferData size = do
  bufferPtr <- (malloc :: IO (Ptr GLuint))
  glGenBuffers 1 bufferPtr
  buffer <- peek bufferPtr
  glBindBuffer target buffer
  glBufferData target size bufferData gl_STATIC_DRAW
  return buffer

attributeId program name = do
  attribute <- withCString name $ \str -> glGetAttribLocation program str
  return $ fromIntegral attribute

uniformId program name = do
  attribute <- withCString name $ \str -> glGetUniformLocation program str
  return $ fromIntegral attribute
