module MiniLisp.Forms(
  lookupForm,
) where

import Control.Monad
import Data.IORef
import MiniLisp.Data
import {-# SOURCE #-} MiniLisp.Environment as E

evaluateBody :: E.Env -> [Value] -> IO Value
evaluateBody env = foldM (\_ -> E.evaluate env) Undef

makeFunction :: E.Env -> String -> [Value] -> [Value] -> Value
makeFunction env name params body =
  Function name func
  where
    func args = do
      funcEnv <- newEnv $ Just env
      initFuncEnv funcEnv
      evaluateBody funcEnv body
      where
        initFuncEnv funcEnv = mapM_ handleParam $ zip params args
          where
            handleParam (Symbol name', value) = do
              var <- E.ensure funcEnv name'
              writeIORef var value
            handleParam _ = error "invalid function call"

formBegin :: E.Env -> [Value] -> IO Value
formBegin = evaluateBody

formQuote :: E.Env -> [Value] -> IO Value
formQuote _ [expr] = return expr
formQuote _ _ = error "quote"

formLet :: E.Env -> [Value] -> IO Value
formLet env (bindings:body) = do
  letEnv <- newEnv $ Just env
  initLetEnv letEnv
  evaluateBody letEnv body
  where
    initLetEnv letEnv = mapM_ handleBinding $ valueToList bindings
      where
        handleBinding binding =
          case valueToList binding of
            [Symbol name, expr] -> do
              value <- E.evaluate env expr
              var <- E.ensure letEnv name
              writeIORef var value
            _ -> error "let"
formLet _ [] = error "let"

formLetStar :: E.Env -> [Value] -> IO Value
formLetStar env (bindings:body) = do
  letEnv <- newEnv $ Just env
  initLetEnv letEnv
  evaluateBody letEnv body
  where
    initLetEnv letEnv = mapM_ handleBinding $ valueToList bindings
      where
        handleBinding binding =
          case valueToList binding of
            [Symbol name, expr] -> do
              value <- E.evaluate letEnv expr
              var <- E.ensure letEnv name
              writeIORef var value
            _ -> error "let*"
formLetStar _ [] = error "let*"

formDefine :: E.Env -> [Value] -> IO Value
formDefine env [Symbol name, expr] = do
  value <- E.evaluate env expr
  var <- E.ensure env name
  writeIORef var value
  return Undef
formDefine env (decl:body) =
  case valueToList decl of
    (Symbol name : params) -> do
      let value = makeFunction env name params body
      var <- E.ensure env name
      writeIORef var value
      return Undef
    _ -> error "define"
formDefine _ _ = error "define"

formLambda :: E.Env -> [Value] -> IO Value
formLambda env (decl:body) =
  return $ makeFunction env "<lambda>" (valueToList decl) body
formLambda _ _ = error "lambda"

formIf :: E.Env -> [Value] -> IO Value
formIf env [testExpr, thenExpr] = do
  testValue <- E.evaluate env testExpr
  if valueToBool testValue then E.evaluate env thenExpr else return Undef
formIf env [testExpr, thenExpr, elseExpr] = do
  testValue <- E.evaluate env testExpr
  E.evaluate env $ if valueToBool testValue then thenExpr else elseExpr
formIf _ _ = error "if"

formCond :: E.Env -> [Value] -> IO Value
formCond _ [] = return Undef
formCond env (branch:restBranches) =
  case valueToList branch of
    (Symbol "else" : body) -> evaluateBody env body
    (testExpr : body) -> do
      testValue <- E.evaluate env testExpr
      if valueToBool testValue
        then evaluateBody env body
        else formCond env restBranches
    _ -> error "cond"

formSet :: E.Env -> [Value] -> IO Value
formSet env [Symbol name, expr] = do
  value <- E.evaluate env expr
  maybeVar <- E.lookup env name
  case maybeVar of
    Just var -> do
      writeIORef var value
      return Undef
    Nothing -> error $ "name not found: " ++ name
formSet _ _ = error "set!"

lookupForm :: String -> Maybe (E.Env -> [Value] -> IO Value)
lookupForm "begin" = Just formBegin
lookupForm "quote" = Just formQuote
lookupForm "let" = Just formLet
lookupForm "let*" = Just formLetStar
lookupForm "define" = Just formDefine
lookupForm "lambda" = Just formLambda
lookupForm "if" = Just formIf
lookupForm "cond" = Just formCond
lookupForm "set!" = Just formSet
lookupForm _ = Nothing
