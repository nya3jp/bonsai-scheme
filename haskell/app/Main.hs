{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Monad
import MiniLisp.Environment
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
  env <- newEnv
  mapM_ (evaluate env) exprs
