import Prelude
import Control.Monad
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW

main = do
  initialize
  openWindow defaultDisplayOptions
  clearColor    $= Color4 1 1 1 1
  blend         $= Enabled
  blendFunc     $= (SrcAlpha, OneMinusSrcAlpha)
  lineSmooth    $= Enabled
  pointSmooth   $= Enabled
  polygonSmooth $= Enabled
  polygonMode   $= (Fill, Fill)
  frontFace     $= CCW
  loop

loop = do
  continue <- windowIsOpen
  when continue $ do
    clear [ ColorBuffer, DepthBuffer ]
    color (Color4 0 0 0 1 :: Color4 GLfloat)
    swapBuffers
    loop
