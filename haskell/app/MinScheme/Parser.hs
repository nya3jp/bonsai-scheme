module MinScheme.Parser( parse ) where

import Data.List
import Data.Maybe
import MinScheme.Data
import Text.Regex

parseSkip :: String -> String
parseSkip code =
  let skipped = head $ fromJust $ matchRegex skipRe code in
  drop (length skipped) code
  where
    skipRe = mkRegex "(^([ \t\r\n]|;.*)*)"

makeQuote :: Value -> IO Value
makeQuote value =
  valueFromList [Symbol "quote", value]

parseValue :: String -> IO (Value, String)
parseValue code =
  case code of
    _ | "'" `isPrefixOf` code -> do
      (value, code') <- parseValue (tail code)
      quoted <- makeQuote value
      return (quoted, code')
    _ | "(" `isPrefixOf` code -> do
      (values, code') <- parseList (tail code)
      let code'' = parseSkip code'
      if ")" `isPrefixOf` code''
        then do
          values' <- valueFromList values
          return (values', tail code'')
        else error $ "parse error: malformed list: " ++ code''
    _ ->
      case matchRegex tokenRe code of
        Nothing -> error $ "parse error: malformed token: " ++ code
        Just (token:_) ->
          let
            code' = drop (length token) code
            value = case token of
              _ | isJust (matchRegex numRe token) -> Integer (read token)
              "#t" -> Boolean True
              "#f" -> Boolean False
              _ -> Symbol token
          in
            return (value, code')
        _ -> undefined
  where
    tokenRe = mkRegex "(^[^ \t\r\n);]+)"
    numRe = mkRegex "(-?[0-9]+$)"

parseList :: String -> IO ([Value], String)
parseList code =
  let code' = parseSkip code in
  case code' of
    "" -> return ([], code')
    _ | ")" `isPrefixOf` code' -> return ([], code')
    _ -> do
      (value, code'') <- parseValue code'
      (values, code''') <- parseList code''
      return (value : values, code''')

parse :: String -> IO [Value]
parse code = do
  (exprs, code') <- parseList code
  if null code' then return exprs else error $ "parse error: excessive code: " ++ code'
