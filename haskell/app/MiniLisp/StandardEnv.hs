module MiniLisp.StandardEnv(newTopLevelEnv) where

import MiniLisp.Builtins
import MiniLisp.Environment

newTopLevelEnv :: IO Env
newTopLevelEnv = do
  env <- newEnv Nothing
  installBuiltins env
  return env
