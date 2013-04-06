import Graphics.Rendering.OpenGL.Raw
import Graphics.UI.GLFW
import Control.Monad

import Version

main = do
  initialize
  openWindow defaultDisplayOptions
  setWindowTitle "Hello World"
  setWindowDimensions 400 300
  glfwVersion <- getGlfwVersion
  glVersion   <- getGlVersion
  case checkVersions glfwVersion glVersion of
    (Left  error)    -> putStrLn error
    (Right versions) -> putStrLn versions >> windowLoop

windowLoop = do
  continue <- windowIsOpen
  when continue $ do
    glClearColor 1 1 1 1
    glClear gl_COLOR_BUFFER_BIT
    swapBuffers
    windowLoop
