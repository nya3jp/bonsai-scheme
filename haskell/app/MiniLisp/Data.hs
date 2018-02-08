module MiniLisp.Data(
  Value(..),
  valueFromList,
  valueToList,
) where

data Value =
  Undef |
  Boolean Bool |
  Integer Int |
  Symbol String |
  Null |
  Pair { car :: Value, cdr :: Value }
  deriving Show

valueFromList :: [Value] -> Value
valueFromList [] = Null
valueFromList (value:rest) = Pair { car = value, cdr = valueFromList rest }

valueToList :: Value -> [Value]
valueToList = undefined
