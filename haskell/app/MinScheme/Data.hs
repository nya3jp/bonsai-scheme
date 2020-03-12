module MinScheme.Data(
  Value(..),
  valueToString,
  valueFromList,
  valueToList,
  valueToBool,
) where

import Data.IORef

data Value =
  Undef |
  Boolean Bool |
  Integer Int |
  Symbol String |
  Null |
  Pair (IORef Value) (IORef Value) |
  Function String ([Value] -> IO Value)

valueToString :: Value -> IO String
valueToString Undef = return "#undef"
valueToString (Boolean False) = return "#f"
valueToString (Boolean True) = return "#t"
valueToString (Integer i) = return $ show i
valueToString (Symbol name) = return name
valueToString Null = return "()"
valueToString (Pair car cdr) = do
  scar <- readIORef car >>= valueToString
  scdr <- readIORef cdr >>= valueToString
  return $ "(" ++ scar ++ " . " ++ scdr ++ ")"
valueToString (Function name _) = return name

valueFromList :: [Value] -> IO Value
valueFromList [] = return Null
valueFromList (x:xs) = do
  lst <- valueFromList xs
  car <- newIORef x
  cdr <- newIORef lst
  return $ Pair car cdr

valueToList :: Value -> IO [Value]
valueToList Null = return []
valueToList (Pair car cdr) = do
  x <- readIORef car
  xs <- readIORef cdr >>= valueToList
  return $ x:xs
valueToList _ = error "not a list"

valueToBool :: Value -> Bool
valueToBool (Boolean False) = False
valueToBool _ = True
