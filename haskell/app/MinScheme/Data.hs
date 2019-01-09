module MinScheme.Data(
  Value(..),
  valueToString,
  valueFromList,
  valueToList,
  valueToBool,
) where

data Value =
  Undef |
  Boolean Bool |
  Integer Int |
  Symbol String |
  Null |
  Pair Value Value |
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
valueFromList = foldr Pair Null

valueToList :: Value -> [Value]
valueToList Null = []
valueToList (Pair car cdr) = car : valueToList cdr
valueToList _ = error "not a list"

valueToBool :: Value -> Bool
valueToBool (Boolean False) = False
valueToBool _ = True
