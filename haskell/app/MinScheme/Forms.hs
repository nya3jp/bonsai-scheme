module MinScheme.Forms(
  lookupForm,
) where

import Control.Monad
import Data.IORef
import MinScheme.Data
import MinScheme.Environment as E
import {-# SOURCE #-} MinScheme.Evaluate

evaluateBody :: E.Env -> [Value] -> IO Value
evaluateBody env = foldM (\_ -> evaluate env) Undef

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
    initLetEnv letEnv = do
      bindings' <- valueToList bindings
      mapM_ handleBinding bindings'
      where
        handleBinding binding = do
          binding' <- valueToList binding
          case binding' of
            [Symbol name, expr] -> do
              value <- evaluate env expr
              var <- E.ensure letEnv name
              writeIORef var value
            _ -> error "let"
formLet _ [] = error "let"

formLetStar :: E.Env -> [Value] -> IO Value
formLetStar env (bindings:body) = do
  letEnv <- initLetEnv env
  evaluateBody letEnv body
  where
    initLetEnv env' = do
      bindings' <- valueToList bindings
      foldM handleBinding env' bindings'
    handleBinding env' binding = do
      env'' <- newEnv $ Just env'
      binding' <- valueToList binding
      case binding' of
        [Symbol name, expr] -> do
          value <- evaluate env' expr
          var <- E.ensure env'' name
          writeIORef var value
        _ -> error "let*"
      return env''
formLetStar _ [] = error "let*"

formLetRec :: E.Env -> [Value] -> IO Value
formLetRec env (bindings:body) = do
  letEnv <- newEnv $ Just env
  initLetEnv letEnv
  evaluateBody letEnv body
  where
    initLetEnv letEnv = do
      bindings' <- valueToList bindings
      mapM_ handleBinding bindings'
      where
        handleBinding binding = do
          binding' <- valueToList binding
          case binding' of
            [Symbol name, expr] -> do
              value <- evaluate letEnv expr
              var <- E.ensure letEnv name
              writeIORef var value
            _ -> error "letrec"
formLetRec _ [] = error "letrec"

formDefine :: E.Env -> [Value] -> IO Value
formDefine env [Symbol name, expr] = do
  value <- evaluate env expr
  var <- E.ensure env name
  writeIORef var value
  return Undef
formDefine env (decl:body) = do
  decl' <- valueToList decl
  case decl' of
    (Symbol name : params) -> do
      let value = makeFunction env name params body
      var <- E.ensure env name
      writeIORef var value
      return Undef
    _ -> error "define"
formDefine _ _ = error "define"

formLambda :: E.Env -> [Value] -> IO Value
formLambda env (decl:body) = do
  decl' <- valueToList decl
  return $ makeFunction env "<lambda>" decl' body
formLambda _ _ = error "lambda"

formIf :: E.Env -> [Value] -> IO Value
formIf env [testExpr, thenExpr] = do
  testValue <- evaluate env testExpr
  if valueToBool testValue then evaluate env thenExpr else return Undef
formIf env [testExpr, thenExpr, elseExpr] = do
  testValue <- evaluate env testExpr
  evaluate env $ if valueToBool testValue then thenExpr else elseExpr
formIf _ _ = error "if"

formCond :: E.Env -> [Value] -> IO Value
formCond _ [] = return Undef
formCond env (branch:restBranches) = do
  branch' <- valueToList branch
  case branch' of
    (Symbol "else" : body) -> evaluateBody env body
    (testExpr : body) -> do
      testValue <- evaluate env testExpr
      if valueToBool testValue
        then evaluateBody env body
        else formCond env restBranches
    _ -> error "cond"

formSet :: E.Env -> [Value] -> IO Value
formSet env [Symbol name, expr] = do
  value <- evaluate env expr
  maybeVar <- E.lookup env name
  case maybeVar of
    Just var -> do
      writeIORef var value
      return Undef
    Nothing -> error $ "name not found: " ++ name
formSet _ _ = error "set!"

formSetCar :: E.Env -> [Value] -> IO Value
formSetCar env [target, value] = do
  target' <- evaluate env target
  value' <- evaluate env value
  case target' of
    Pair car _ -> do
      writeIORef car value'
      return Undef
    _ -> error "not pair"
formSetCar _ _ = error "set-car!"

formSetCdr :: E.Env -> [Value] -> IO Value
formSetCdr env [target, value] = do
  target' <- evaluate env target
  value' <- evaluate env value
  case target' of
    Pair _ cdr -> do
      writeIORef cdr value'
      return Undef
    _ -> error "not pair"
formSetCdr _ _ = error "set-cdr!"

lookupForm :: String -> Maybe (E.Env -> [Value] -> IO Value)
lookupForm "begin" = Just formBegin
lookupForm "quote" = Just formQuote
lookupForm "let" = Just formLet
lookupForm "let*" = Just formLetStar
lookupForm "letrec" = Just formLetRec
lookupForm "define" = Just formDefine
lookupForm "lambda" = Just formLambda
lookupForm "if" = Just formIf
lookupForm "cond" = Just formCond
lookupForm "set!" = Just formSet
lookupForm "set-car!" = Just formSetCar
lookupForm "set-cdr!" = Just formSetCdr
lookupForm _ = Nothing
