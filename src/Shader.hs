module Shader (createProgram) where

import Foreign
import Foreign.C.String
import Graphics.Rendering.OpenGL.Raw

createProgram shader = do
  vertexShader   <- createShader gl_VERTEX_SHADER   $ concat ["../../shaders/", shader, ".v.glsl"]
  fragmentShader <- createShader gl_FRAGMENT_SHADER $ concat ["../../shaders/", shader, ".f.glsl"]
  program        <- compileProgram [vertexShader, fragmentShader]
  return program

compileProgram shaders = do
  alloca $ \programOk -> do
    program <- glCreateProgram
    mapM_ (glAttachShader program) shaders
    glLinkProgram program
    glGetProgramiv program gl_LINK_STATUS programOk
    programOkVal <- peek programOk
    if programOkVal == 0
      then do
        logLengthPointer <- malloc :: IO (Ptr GLint)
        glGetProgramiv program gl_INFO_LOG_LENGTH logLengthPointer
        logLength <- peek logLengthPointer
        logMessagePointer <- mallocArray (fromIntegral logLength) :: IO (Ptr GLchar)
        glGetProgramInfoLog program logLength nullPtr logMessagePointer
        logMessage <- peekCString logMessagePointer
        error $ "Program failed to compile" ++ "\n" ++ logMessage
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
            then do
              logLengthPointer <- malloc :: IO (Ptr GLint)
              glGetShaderiv shader gl_INFO_LOG_LENGTH logLengthPointer
              logLength <- peek logLengthPointer
              logMessagePointer <- mallocArray (fromIntegral logLength) :: IO (Ptr GLchar)
              glGetShaderInfoLog shader logLength nullPtr logMessagePointer
              logMessage <- peekCString logMessagePointer
              error $ "Shader " ++ filename ++ " failed to compile" ++ "\n" ++ logMessage
            else return shader
