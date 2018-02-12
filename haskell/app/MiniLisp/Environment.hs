module MiniLisp.Environment(
  newTopLevelEnv,
  evaluate,
) where

import Data.IORef
import qualified Data.Map.Lazy as M
import MiniLisp.Builtins
import MiniLisp.Data
import {-# SOURCE #-} MiniLisp.Forms

newTopLevelEnv :: IO Env
newTopLevelEnv = do
  env <- newEnv Nothing
  case env of Env _ vars -> installBuiltins vars
  return env

lookupEnv :: Env -> String -> IO (Maybe Var)
lookupEnv (Env parent vars) name = do
  m <- readIORef vars
  case M.lookup name m of
    Just var -> return $ Just var
    Nothing -> case parent of
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
