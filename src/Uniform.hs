{-# LANGUAGE FlexibleInstances #-}

module Uniform where

import Renderable
import Graphics.Rendering.OpenGL.Raw
import Foreign.C.String

data Uniform            = Uniform3Floats String GLint GLfloat GLfloat GLfloat deriving (Show)
data UniformDescription = Uniform3FloatsDescription String GLfloat GLfloat GLfloat deriving (Show)

instance Renderable Uniform where
  render (Uniform3Floats _ uid a b c) = glUniform3f uid a b c

createUniform :: GLuint -> UniformDescription -> IO Uniform
createUniform programId (Uniform3FloatsDescription name a b c) = do
  uniformId <- uniformId programId name
  return $ Uniform3Floats name uniformId a b c

uniformId program name = do
  attribute <- withCString name $ \str -> glGetUniformLocation program str
  return $ fromIntegral attribute
