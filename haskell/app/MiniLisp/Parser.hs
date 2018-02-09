module MiniLisp.Parser( parse ) where

import Data.List
import Data.Maybe
import MiniLisp.Data
import Text.Regex

parseSkip :: String -> String
parseSkip code =
  let skipped = head $ fromJust $ matchRegex skipRe code in
  drop (length skipped) code
  where
    skipRe = mkRegex "(^([ \t\r\n]|;.*)*)"

makeQuote :: Value -> Value
makeQuote value =
  valueFromList [Symbol "quote", value]

parseValue :: String -> (Value, String)
parseValue code =
  case code of
    _ | "'" `isPrefixOf` code ->
      let (value, code') = parseValue (tail code) in
      (makeQuote value, code')
    _ | "(" `isPrefixOf` code ->
      let (values, code') = parseList (tail code) in
      let code'' = parseSkip code' in
      if ")" `isPrefixOf` code''
        then (valueFromList values, tail code'')
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
            (value, code')
        _ -> undefined
  where
    tokenRe = mkRegex "(^[^ \t\r\n);]+)"
    numRe = mkRegex "(-?[0-9]+$)"

parseList :: String -> ([Value], String)
parseList code =
  let code' = parseSkip code in
  case code' of
    "" -> ([], code')
    _ | ")" `isPrefixOf` code' -> ([], code')
    _ ->
      let (value, code'') = parseValue code' in
      let (values, code''') = parseList code'' in
      (value : values, code''')

parse :: String -> [Value]
parse code =
  let (exprs, code') = parseList code in
  if null code' then exprs else error $ "parse error: excessive code: " ++ code'
