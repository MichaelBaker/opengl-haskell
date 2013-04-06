module Version where

import Data.Version
import Control.Monad.Instances
import Data.List

checkVersions glfwVersion glVersion = validateVersions (versionBranch glfwVersion) (versionBranch glVersion)

validateVersions glfw gl = do
  glfwMessage <- atLeastVersion 2 "GLFW"   glfw
  glMessage   <- atLeastVersion 2 "OpenGL" gl
  return $ unlines [glfwMessage, glMessage]

atLeastVersion _   name []     = Left ("Could not determine version of " ++ name)
atLeastVersion min name (v:vs) | v >= min  = Right (unwords ["[Version]", name, version])
                               | otherwise = Left  (unwords ["[Error][Version]", name, version, "<", show min])
                               where version = intercalate "." $ map show (v:vs)

