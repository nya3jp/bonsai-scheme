module MinScheme.Evaluate(evaluate) where

import Data.IORef
import MinScheme.Data
import MinScheme.Environment
import MinScheme.Forms
import Prelude hiding (lookup)

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
    Pair car cdr -> do
      car' <- readIORef car
      cdr' <- readIORef cdr
      case lookupFormByValue car' of
        Just form -> do
          args <- valueToList cdr'
          form env args
        Nothing -> do
          value <- evaluate env car'
          args <- valueToList cdr'
          args' <- mapM (evaluate env) args
          case value of
            Function _ f -> f args'
            _ -> error "not a function"
      where
        lookupFormByValue (Symbol name) = lookupForm name
        lookupFormByValue _ = Nothing
    _ -> error "failed to evaluate"
