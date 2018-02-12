module Main where

import Control.Monad
import MiniLisp.Environment as E
import MiniLisp.Parser
import System.Environment

readCode :: IO String
readCode = do
  args <- getArgs
  guard $ length args == 1
  let filename = head args
  readFile filename

main :: IO ()
main = do
  code <- readCode
  let exprs = parse code
  env <- E.newTopLevelEnv
  mapM_ (E.evaluate env) exprs
