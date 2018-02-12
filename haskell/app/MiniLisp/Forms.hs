module MiniLisp.Forms(
  lookupForm,
) where

import Control.Monad
import Data.IORef
import qualified Data.Map.Lazy as M
import MiniLisp.Data
import MiniLisp.Environment

evaluateBody :: Env -> [Value] -> IO Value
evaluateBody env = foldM (\_ -> evaluate env) Undef

setVariable :: IORef VarMap -> String -> Value -> IO ()
setVariable vars name value = do
  m <- readIORef vars
  var <- newIORef value
  let m' = M.insert name var m
  writeIORef vars m'

formBegin :: Env -> [Value] -> IO Value
formBegin = evaluateBody

formQuote :: Env -> [Value] -> IO Value
formQuote _ [expr] = return expr
formQuote _ _ = error "quote"

formLet :: Env -> [Value] -> IO Value
formLet env (bindings:body) = do
  letEnv <- newEnv $ Just env
  initLetEnv letEnv
  evaluateBody letEnv body
  where
    initLetEnv (Env _ vars) = mapM_ handleBinding $ valueToList bindings
      where
        handleBinding binding =
          case valueToList binding of
            [Symbol name, expr] -> do
              value <- evaluate env expr
              setVariable vars name value
            _ -> error "let"
formLet _ [] = error "let"

formLetStar :: Env -> [Value] -> IO Value
formLetStar env (bindings:body) = do
  letEnv <- newEnv $ Just env
  initLetEnv letEnv
  evaluateBody letEnv body
  where
    initLetEnv letEnv@(Env _ vars) = mapM_ handleBinding $ valueToList bindings
      where
        handleBinding binding =
          case valueToList binding of
            [Symbol name, expr] -> do
              value <- evaluate letEnv expr
              setVariable vars name value
            _ -> error "let*"
formLetStar _ [] = error "let*"

lookupForm :: String -> Maybe (Env -> [Value] -> IO Value)
lookupForm "begin" = Just formBegin
lookupForm "quote" = Just formQuote
lookupForm "let" = Just formLet
lookupForm "let*" = Just formLetStar
lookupForm _ = Nothing
