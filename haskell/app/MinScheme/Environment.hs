module MinScheme.Environment(
  Var,
  Env,
  newEnv,
  ensure,
  lookup,
) where

import Data.IORef
import qualified Data.Map.Lazy as M
import MinScheme.Data
import Prelude hiding (lookup)

type Var = IORef Value
type VarMap = M.Map String Var

data Env = Env (Maybe Env) (IORef VarMap)

newEnv :: Maybe Env -> IO Env
newEnv parent = do
  vars <- newIORef M.empty
  return $ Env parent vars

ensure :: Env -> String -> IO Var
ensure (Env _ vars) name = do
  vars' <- readIORef vars
  case M.lookup name vars' of
    Just var -> return var
    Nothing -> do
      var <- newIORef Undef
      let map' = M.insert name var vars'
      writeIORef vars map'
      return var

lookup :: Env -> String -> IO (Maybe Var)
lookup (Env parent vars) name = do
  vars' <- readIORef vars
  case M.lookup name vars' of
    Just var -> return $ Just var
    Nothing -> case parent of
      Just p -> lookup p name
      Nothing -> return Nothing
