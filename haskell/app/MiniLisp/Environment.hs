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
import MiniLisp.Builtins
import MiniLisp.Data
import MiniLisp.Forms
import Prelude hiding (lookup)

type Var = IORef Value
type VarMap = M.Map String Var

data Env = Env (Maybe Env) (IORef VarMap)

newEnv :: Maybe Env -> IO Env
newEnv parent = do
  vars <- newIORef M.empty
  return $ Env parent vars

newTopLevelEnv :: IO Env
newTopLevelEnv = do
  env <- newEnv Nothing
  installBuiltins env
  return env

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

evaluate :: Env -> Value -> IO Value
evaluate env expr =
  case expr of
    Null -> return expr
    Boolean _ -> return expr
    Integer _ -> return expr
    Symbol name -> do
      result <- lookup env name
      case result of
        Just var -> readIORef var
        Nothing -> error $ "name not found: " ++ name
    Pair car cdr ->
      case lookupFormByValue car of
        Just form -> form env (valueToList cdr)
        Nothing -> do
          value <- evaluate env car
          args <- mapM (evaluate env) (valueToList cdr)
          case value of
            Function _ f -> f args
            _ -> error "not a function"
      where
        lookupFormByValue (Symbol name) = lookupForm name
        lookupFormByValue _ = Nothing
    _ -> error "failed to evaluate"
