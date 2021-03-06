module MinScheme.Builtins(
  installBuiltins,
) where

import Data.IORef
import MinScheme.Data
import MinScheme.Environment

valueToInt :: Value -> Int
valueToInt (Integer a) = a
valueToInt _ = error "not an integer"

builtinPrint :: [Value] -> IO Value
builtinPrint [arg] = do
  valueToString arg >>= putStrLn
  return Undef
builtinPrint _ = error "print"

builtinAnd :: [Value] -> Value
builtinAnd args = Boolean $ all valueToBool args

builtinOr :: [Value] -> Value
builtinOr args = Boolean $ any valueToBool args

builtinNot :: [Value] -> Value
builtinNot [arg] = Boolean $ not $ valueToBool arg
builtinNot _ = error "not"

builtinEq :: [Value] -> Value
builtinEq [Integer a, Integer b] = Boolean $ a == b
builtinEq _ = error "=="

builtinLt :: [Value] -> Value
builtinLt [Integer a, Integer b] = Boolean $ a < b
builtinLt _ = error "<"

builtinLte :: [Value] -> Value
builtinLte [Integer a, Integer b] = Boolean $ a <= b
builtinLte _ = error "<="

builtinGt :: [Value] -> Value
builtinGt [Integer a, Integer b] = Boolean $ a > b
builtinGt _ = error ">"

builtinGte :: [Value] -> Value
builtinGte [Integer a, Integer b] = Boolean $ a >= b
builtinGte _ = error ">="

builtinAdd :: [Value] -> Value
builtinAdd values = Integer $ sum $ map valueToInt values

builtinSub :: [Value] -> Value
builtinSub (firstValue:restValues) = Integer $ (valueToInt firstValue -) $ sum $ map valueToInt restValues
builtinSub [] = error "-"

builtinMul :: [Value] -> Value
builtinMul values = Integer $ product $ map valueToInt values

builtinDiv :: [Value] -> Value
builtinDiv (firstValue:restValues) = Integer $ (valueToInt firstValue `div`) $ product $ map valueToInt restValues
builtinDiv [] = error "/"

builtinEqCheck :: [Value] -> Value
builtinEqCheck [Boolean a, Boolean b] | a == b = Boolean True
builtinEqCheck [Integer a, Integer b] | a == b = Boolean True
builtinEqCheck [Symbol a, Symbol b] | a == b = Boolean True
builtinEqCheck [Null, Null] = Boolean True
builtinEqCheck [_, _] = Boolean False
builtinEqCheck _ = error "eq?"

builtinCons :: [Value] -> IO Value
builtinCons [car, cdr] = do
  car' <- newIORef car
  cdr' <- newIORef cdr
  return $ Pair car' cdr'
builtinCons _ = error "cons"

builtinCar :: [Value] -> IO Value
builtinCar [Pair car _] = readIORef car
builtinCar _ = error "car"

builtinCdr :: [Value] -> IO Value
builtinCdr [Pair _ cdr] = readIORef cdr
builtinCdr _ = error "cdr"

installBuiltins :: Env -> IO ()
installBuiltins env = do
  installIO "print" builtinPrint
  installPure "and" builtinAnd
  installPure "or" builtinOr
  installPure "not" builtinNot
  installPure "=" builtinEq
  installPure "<" builtinLt
  installPure "<=" builtinLte
  installPure ">" builtinGt
  installPure ">=" builtinGte
  installPure "+" builtinAdd
  installPure "-" builtinSub
  installPure "*" builtinMul
  installPure "/" builtinDiv
  installPure "eq?" builtinEqCheck
  installIO "cons" builtinCons
  installIO "car" builtinCar
  installIO "cdr" builtinCdr
  where
    installIO name func = do
      var <- ensure env name
      writeIORef var $ Function name func
    installPure name func = installIO name $ return . func
