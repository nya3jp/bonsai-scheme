module Main where

import Control.Monad
import MinScheme.Evaluate
import MinScheme.StandardEnv
import MinScheme.Parser
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
  env <- newTopLevelEnv
  mapM_ (evaluate env) exprs
