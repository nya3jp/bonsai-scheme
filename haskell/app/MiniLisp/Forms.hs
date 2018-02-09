module MiniLisp.Forms(
  lookupForm,
) where

import Control.Monad
import MiniLisp.Data
import MiniLisp.Environment

evaluateBody :: Env -> [Value] -> IO Value
evaluateBody env = foldM (\_ -> evaluate env) Undef

formBegin :: Env -> [Value] -> IO Value
formBegin = evaluateBody

formQuote :: Env -> [Value] -> IO Value
formQuote _ exprs = do
  guard $ length exprs == 1
  return $ head exprs

lookupForm :: String -> Maybe (Env -> [Value] -> IO Value)
lookupForm "begin" = Just formBegin
lookupForm "quote" = Just formQuote
lookupForm _ = Nothing
