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

makeFunction :: Env -> String -> [Value] -> [Value] -> Value
makeFunction env name params body =
  Function name func
  where
    func args = do
      funcEnv <- newEnv $ Just env
      initFuncEnv funcEnv
      evaluateBody funcEnv body
      where
        initFuncEnv (Env _ vars) = mapM_ handleParam $ zip params args
          where
            handleParam (Symbol name', value) = setVariable vars name' value
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

formDefine :: Env -> [Value] -> IO Value
formDefine env@(Env _ vars) [Symbol name, expr] = do
  value <- evaluate env expr
  setVariable vars name value
  return Undef
formDefine env@(Env _ vars) (decl:body) =
  case valueToList decl of
    (Symbol name : params) -> do
      let value = makeFunction env name params body
      setVariable vars name value
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

lookupForm :: String -> Maybe (Env -> [Value] -> IO Value)
lookupForm "begin" = Just formBegin
lookupForm "quote" = Just formQuote
lookupForm "let" = Just formLet
lookupForm "let*" = Just formLetStar
lookupForm "define" = Just formDefine
lookupForm "lambda" = Just formLambda
lookupForm "if" = Just formIf
lookupForm "cond" = Just formCond
lookupForm _ = Nothing
