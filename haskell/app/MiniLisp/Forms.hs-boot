module MiniLisp.Forms(
  lookupForm,
) where

import MiniLisp.Data

lookupForm :: String -> Maybe (Env -> [Value] -> IO Value)
