{-# LANGUAGE FlexibleInstances #-}

module Uniform where

import Renderable
import Graphics.Rendering.OpenGL.Raw
import Foreign.C.String

data Uniform = Uniform3Floats String GLint GLfloat GLfloat GLfloat
             | Uniform2Floats String GLint GLfloat GLfloat
             | UniformFloat   String GLint GLfloat
             | UniformInt     String GLint GLint
             deriving (Show)

data UniformDescription = Uniform3FloatsDescription String GLfloat GLfloat GLfloat
                        | Uniform2FloatsDescription String GLfloat GLfloat
                        | UniformFloatDescription   String GLfloat
                        | UniformIntDescription     String GLint
                        deriving (Show)

instance Renderable Uniform where
  render (Uniform3Floats _ uid a b c) = glUniform3f uid a b c
  render (Uniform2Floats _ uid a b)   = glUniform2f uid a b
  render (UniformFloat   _ uid a)     = glUniform1f uid a
  render (UniformInt     _ uid a)     = glUniform1i uid a

uid (Uniform3Floats _ uid _ _ _) = uid
uid (Uniform2Floats _ uid _ _)   = uid
uid (UniformFloat   _ uid _)     = uid
uid (UniformInt     _ uid _)     = uid

createUniform :: GLuint -> UniformDescription -> IO Uniform
createUniform programId (Uniform3FloatsDescription name a b c) = do
  uniformId <- uniformId programId name
  return $ Uniform3Floats name uniformId a b c
createUniform programId (Uniform2FloatsDescription name a b) = do
  uniformId <- uniformId programId name
  return $ Uniform2Floats name uniformId a b
createUniform programId (UniformFloatDescription name a) = do
  uniformId <- uniformId programId name
  return $ UniformFloat name uniformId a
createUniform programId (UniformIntDescription name a) = do
  uniformId <- uniformId programId name
  return $ UniformInt name uniformId a

uniformId program name = do
  attribute <- withCString name $ \str -> glGetUniformLocation program str
  return $ fromIntegral attribute
