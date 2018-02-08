module MiniLisp.Environment(
  newEnv,
  evaluate,
) where

import Data.IORef
import Data.Map.Lazy
import MiniLisp.Builtins
import MiniLisp.Data
import Prelude hiding (lookup)

newEnv :: IO Env
newEnv = do
  vars <- newIORef empty
  installBuiltins vars
  return Env { parent = Nothing, vars = vars }

lookupEnv :: Env -> String -> IO (Maybe Var)
lookupEnv env name = do
  m <- readIORef $ vars env
  case lookup name m of
    Just var -> return $ Just var
    Nothing -> case parent env of
      Just p -> lookupEnv p name
      Nothing -> return Nothing

evaluate :: Env -> Value -> IO Value
evaluate env expr =
  case expr of
    Null -> return expr
    Boolean _ -> return expr
    Integer _ -> return expr
    Symbol name -> do
      result <- lookupEnv env name
      case result of
        Just var -> readIORef var
        Nothing -> error "name not found"
    Pair car cdr -> do
      value <- evaluate env car
      args <- mapM (evaluate env) (valueToList cdr)
      case value of
        Function _ f -> f args
        _ -> error "not a function"
    _ -> error "failed to evaluate"
