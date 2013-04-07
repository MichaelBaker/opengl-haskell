import Graphics.Rendering.OpenGL.Raw
import Graphics.UI.GLFW
import Control.Monad

import Version
import Resources

main = do
  initialize
  openWindow defaultDisplayOptions
  setWindowTitle "Hello World"

  glfwVersion <- getGlfwVersion
  glVersion   <- getGlVersion

  case checkVersions glfwVersion glVersion of
    (Left  error)    -> putStrLn error
    (Right versions) -> do
      putStrLn versions
      resources <- createResources
      windowLoop resources

windowLoop resources = do
  continue <- windowIsOpen
  when continue $ do
    glClearColor 1 1 1 1
    glClear gl_COLOR_BUFFER_BIT

    glUseProgram           $ program   resources
    enableAttributePointer $ verticies resources
    drawElements           $ elements  resources

    disableAttributePointer $ verticies resources

    swapBuffers
    windowLoop resources
