module MiniLisp.Builtins where

import Data.IORef
import qualified Data.Map.Lazy as M
import MiniLisp.Data

valueToInt :: Value -> Int
valueToInt (Integer a) = a
valueToInt _ = error "not an integer"

builtinPrint :: [Value] -> IO Value
builtinPrint [arg] = do
  putStrLn $ valueToString arg
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

builtinCons :: [Value] -> Value
builtinCons [car, cdr] = Pair car cdr
builtinCons _ = error "cons"

builtinCar :: [Value] -> Value
builtinCar [Pair car _] = car
builtinCar _ = error "car"

builtinCdr :: [Value] -> Value
builtinCdr [Pair _ cdr] = cdr
builtinCdr _ = error "cdr"

installBuiltins :: IORef VarMap -> IO ()
installBuiltins vars = do
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
  installPure "cons" builtinCons
  installPure "car" builtinCar
  installPure "cdr" builtinCdr
  where
    installIO name func = do
      m <- readIORef vars
      var <- newIORef $ Function name func
      let m' = M.insert name var m
      writeIORef vars m'
    installPure name func = installIO name $ return . func
