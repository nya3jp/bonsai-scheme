module MiniLisp.Builtins where

import Control.Monad
import Data.IORef
import qualified Data.Map.Lazy as M
import MiniLisp.Data

builtinPrint :: [Value] -> IO Value
builtinPrint args = do
  guard $ length args == 1
  putStrLn $ valueToString $ head args
  return Undef

installBuiltins :: IORef VarMap -> IO ()
installBuiltins vars = do
  install "print" builtinPrint
  where
    install name func = do
      m <- readIORef vars
      var <- newIORef $ Function name func
      let m' = M.insert name var m
      writeIORef vars m'
