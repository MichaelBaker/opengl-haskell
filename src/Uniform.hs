{-# LANGUAGE FlexibleInstances #-}

module Uniform where

import Renderable
import Graphics.Rendering.OpenGL.Raw
import Foreign.C.String

data Uniform a = Uniform String GLint a

setUniform   (Uniform name uid _) value    = Uniform name uid value
alterUniform (Uniform name uid oldValue) f = Uniform name uid $ f oldValue

instance Renderable (Uniform GLfloat) where
  render (Uniform _ uid value) = glUniform1f uid value

instance Renderable (Uniform GLint) where
  render (Uniform _ uid value) = glUniform1i uid value

createUniformFloat :: GLuint -> String -> GLfloat -> IO (Uniform GLfloat)
createUniformFloat program name value = do
  uid <- createUniform program name
  return $ Uniform name uid value

createUniformInt :: GLuint -> String -> GLint -> IO (Uniform GLint)
createUniformInt   program name value = do
  uid <- createUniform program name
  return $ Uniform name uid value

createUniform :: GLuint -> String -> IO GLint
createUniform = uniformId

uniformId program name = do
  attribute <- withCString name $ \str -> glGetUniformLocation program str
  return $ fromIntegral attribute
