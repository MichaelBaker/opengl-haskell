module Job where

import Graphics.Rendering.OpenGL.Raw

import Renderable
import Element
import Attribute
import Uniform

data Job = Job { program    :: GLuint
               , attributes :: [Attribute]
               , elements   :: ElementArray
               , uniforms   :: [Uniform]
               } deriving (Show)

instance Renderable Job where
  render (Job program attributes elements uniforms) = do
      glUseProgram           program
      mapM_ render           uniforms
      mapM_ enableAttribute  attributes
      drawElements           elements
      mapM_ disableAttribute attributes
