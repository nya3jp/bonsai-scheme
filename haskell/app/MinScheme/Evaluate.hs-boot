module MinScheme.Evaluate(evaluate) where

import MinScheme.Data
import MinScheme.Environment

evaluate :: Env -> Value -> IO Value
