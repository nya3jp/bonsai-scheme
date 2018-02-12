module MiniLisp.Forms(
  lookupForm,
) where

import Control.Monad
import Data.IORef
import qualified Data.Map.Lazy as M
import MiniLisp.Data
import MiniLisp.Environment as E

evaluateBody :: Env -> [Value] -> IO Value
evaluateBody env = foldM (\_ -> evaluate env) Undef

makeFunction :: Env -> String -> [Value] -> [Value] -> Value
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
    initLetEnv letEnv = mapM_ handleBinding $ valueToList bindings
      where
        handleBinding binding =
          case valueToList binding of
            [Symbol name, expr] -> do
              value <- evaluate env expr
              var <- E.ensure letEnv name
              writeIORef var value
            _ -> error "let"
formLet _ [] = error "let"

formLetStar :: Env -> [Value] -> IO Value
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
              value <- evaluate letEnv expr
              var <- E.ensure letEnv name
              writeIORef var value
            _ -> error "let*"
formLetStar _ [] = error "let*"

formDefine :: Env -> [Value] -> IO Value
formDefine env [Symbol name, expr] = do
  value <- evaluate env expr
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

formLambda :: Env -> [Value] -> IO Value
formLambda env (decl:body) =
  return $ makeFunction env "<lambda>" (valueToList decl) body
formLambda _ _ = error "lambda"

formIf :: Env -> [Value] -> IO Value
formIf env [testExpr, thenExpr] = do
  testValue <- evaluate env testExpr
  if valueToBool testValue then evaluate env thenExpr else return Undef
formIf env [testExpr, thenExpr, elseExpr] = do
  testValue <- evaluate env testExpr
  evaluate env $ if valueToBool testValue then thenExpr else elseExpr
formIf _ _ = error "if"

formCond :: Env -> [Value] -> IO Value
formCond _ [] = return Undef
formCond env (branch:restBranches) =
  case valueToList branch of
    (Symbol "else" : body) -> evaluateBody env body
    (testExpr : body) -> do
      testValue <- evaluate env testExpr
      if valueToBool testValue
        then evaluateBody env body
        else formCond env restBranches
    _ -> error "cond"

formSet :: Env -> [Value] -> IO Value
formSet env [Symbol name, expr] = do
  value <- evaluate env expr
  maybeVar <- E.lookup env name
  case maybeVar of
    Just var -> do
      writeIORef var value
      return Undef
    Nothing -> error $ "name not found: " ++ name
formSet _ _ = error "set!"

lookupForm :: String -> Maybe (Env -> [Value] -> IO Value)
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
