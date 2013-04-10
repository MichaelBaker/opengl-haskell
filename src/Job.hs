module Job where

import Graphics.Rendering.OpenGL.Raw

import Resources

data Job = Job { program    :: GLuint
               , attributes :: [AttributeArray]
               , elements   :: ElementArray
               }

render (Job program attributes elements) = do
    glUseProgram           program
    mapM_ enableAttribute  attributes
    drawElements           elements
    mapM_ disableAttribute attributes
