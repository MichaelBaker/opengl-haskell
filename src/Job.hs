module Job where

import Graphics.Rendering.OpenGL.Raw

import Resources
import Renderable

data Job = Job { program    :: GLuint
               , attributes :: [AttributeArray]
               , elements   :: ElementArray
               } deriving (Show)

instance Renderable Job where
  render (Job program attributes elements) = do
      glUseProgram           program
      mapM_ enableAttribute  attributes
      drawElements           elements
      mapM_ disableAttribute attributes
