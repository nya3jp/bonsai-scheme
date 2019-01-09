module MiniLisp.Evaluate(evaluate) where

import MiniLisp.Data
import MiniLisp.Environment

evaluate :: Env -> Value -> IO Value
