module MiniLisp.Data(
  Value(..),
  valueToString,
  valueFromList,
  valueToList,
  Var,
  VarMap,
  Env(Env, parent, vars),
) where

import Data.IORef
import Data.Map.Lazy

data Value =
  Undef |
  Boolean Bool |
  Integer Int |
  Symbol String |
  Null |
  Pair { car :: Value, cdr :: Value } |
  Function String ([Value] -> IO Value)

valueToString :: Value -> String
valueToString Undef = "#undef"
valueToString (Boolean False) = "#f"
valueToString (Boolean True) = "#t"
valueToString (Integer i) = show i
valueToString (Symbol name) = name
valueToString Null = "()"
valueToString (Pair car cdr) = "(" ++ valueToString car ++ " . " ++ valueToString cdr ++ ")"
valueToString (Function name _) = name

valueFromList :: [Value] -> Value
valueFromList [] = Null
valueFromList (value:rest) = Pair { car = value, cdr = valueFromList rest }

valueToList :: Value -> [Value]
valueToList Null = []
valueToList (Pair car cdr) = car : valueToList cdr
valueToList _ = error "not a list"

type Var = IORef Value
type VarMap = Map String Var

data Env = Env {
  parent :: Maybe Env,
  vars :: IORef VarMap
}
