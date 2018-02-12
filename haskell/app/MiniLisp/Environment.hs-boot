module MiniLisp.Environment(
  Var,
  Env,
  newEnv,
  newTopLevelEnv,
  ensure,
  lookup,
  evaluate,
) where

import Data.IORef
import qualified Data.Map.Lazy as M
import MiniLisp.Data
import Prelude hiding (lookup)

type Var = IORef Value
type VarMap = M.Map String Var

data Env = Env (Maybe Env) (IORef VarMap)

newEnv :: Maybe Env -> IO Env

newTopLevelEnv :: IO Env

ensure :: Env -> String -> IO Var

lookup :: Env -> String -> IO (Maybe Var)

evaluate :: Env -> Value -> IO Value
