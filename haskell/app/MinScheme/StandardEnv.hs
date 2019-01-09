module MinScheme.StandardEnv(newTopLevelEnv) where

import MinScheme.Builtins
import MinScheme.Environment

newTopLevelEnv :: IO Env
newTopLevelEnv = do
  env <- newEnv Nothing
  installBuiltins env
  return env
