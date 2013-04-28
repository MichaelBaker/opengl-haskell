module Resources where

import Foreign
import Foreign.C.String
import Graphics.Rendering.OpenGL.Raw

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

type Vertex = (GLfloat, GLfloat, GLfloat, GLfloat)

type Color = (GLfloat, GLfloat, GLfloat, GLfloat)

printVertexArray [] = return ()
printVertexArray array = do
  print $ take 4 array
  printVertexArray $ drop 4 array

createGenericAttributes :: GLuint -> [GLfloat] -> IO [AttributeArray]
createGenericAttributes program verticies = do
  position    <- attributeId program "position"
  faceColor   <- attributeId program "faceColor"
  translation <- attributeId program "translation"
  normal      <- attributeId program "normal"

  vertexArrayPtr <- newArray verticies
  vertexBuffer   <- createBuffer gl_ARRAY_BUFFER vertexArrayPtr (listSize verticies)

  let positions    = AttributeArray vertexBuffer position    4 gl_FLOAT (fromIntegral $ 16 * floatSize) nullPtr
      faceColors   = AttributeArray vertexBuffer faceColor   4 gl_FLOAT (fromIntegral $ 16 * floatSize) (createOffset floatSize 4)
      translations = AttributeArray vertexBuffer translation 4 gl_FLOAT (fromIntegral $ 16 * floatSize) (createOffset floatSize 8)
      normals      = AttributeArray vertexBuffer normal      4 gl_FLOAT (fromIntegral $ 16 * floatSize) (createOffset floatSize 12)

  return [positions, faceColors, translations, normals]


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

floatSize = sizeOf (0 :: GLfloat)

createOffset typeSize amount = plusPtr nullPtr $ typeSize * amount

cross (a, b, c, _) (x, y, z, _) = (b*z - c*y, c*x - a*z, a*y - b*x, 1.0)

minus     (a, b, c, _) (x, y, z, _) = (a - x, b - y, c - z, 1.0)
plus      (a, b, c, _) (x, y, z, _) = (a + x, b + y, c + z, 1.0)
midpoint  (a, b, c, _) (x, y, z, _) = ((a+x)/2.0, (b+y)/2.0, (c+z)/2.0, 1.0)
normalize (x, y, z, w)              = (x/l, y/l, z/l, w) where l = sqrt $ (x^2) + (y^2) + (z^2)

detuple (a, b, c, d) = [a, b, c, d]

flattenVerticies = concat . (map detuple)

listSize list = fromIntegral $ sizeOf (head list) * length list

createProgram shader = do
  vertexShader   <- createShader gl_VERTEX_SHADER   $ concat ["../../shaders/", shader, ".v.glsl"]
  fragmentShader <- createShader gl_FRAGMENT_SHADER $ concat ["../../shaders/", shader, ".f.glsl"]
  program        <- compileProgram [vertexShader, fragmentShader]
  return program

createElements :: [GLshort] -> IO ElementArray
createElements items = do
  elementArrayPtr <- newArray items
  elementBuffer   <- createBuffer gl_ELEMENT_ARRAY_BUFFER elementArrayPtr (listSize items)
  return $ ElementArray elementBuffer gl_TRIANGLES (fromIntegral $ length items) gl_UNSIGNED_SHORT

drawElements elementArray = do
  glBindBuffer gl_ELEMENT_ARRAY_BUFFER (elementBuffer elementArray)
  glDrawElements
    (triangleType elementArray)
    (vertexCount  elementArray)
    (indexType    elementArray)
    nullPtr

compileProgram shaders = do
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
