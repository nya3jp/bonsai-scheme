{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Monad
import MiniLisp.Parser
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  guard $ length args == 1
  let filename = head args
  code <- readFile filename
  let exprs = parse code
  print exprs
