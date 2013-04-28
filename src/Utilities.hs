module Utilities where

when predicate action = do
  shouldRun <- predicate
  case shouldRun of
    True  -> action
    False -> return ()
