module GlobRegex
    (
      globToRegex
    , GlobError(..)
    , matchesGlob
    ) where

import Data.Char (toLower)
import Text.Regex.Posix ((=~))
import System.FilePath (pathSeparator)

type GlobError = String

globToRegex :: String -> Bool -> Either GlobError String
globToRegex cs caseSensitive =
    case res of
      (Right res') -> Right $ '^' : res' ++ "$"
      (Left e)     -> Left e
    where cs' = if caseSensitive then cs else map toLower cs
          res = globToRegex' cs'

globToRegex' :: String -> Either GlobError String
globToRegex' ""             = Right ""

globToRegex' ('*':'*':cs)   =
  case res of
    (Left _) -> res
    (Right res') -> Right $ ".*/" ++ res'
  where res = (globToRegex' . stripSlashes) cs

globToRegex' ('*':cs)       =
  case res of
    (Left _) -> res
    (Right res') -> Right $ ".*" ++ res'
  where res = globToRegex' cs

globToRegex' ('?':cs)       = 
  case res of
    (Left _) -> res
    (Right res') -> Right $ '.' : res'
  where res = globToRegex' cs

globToRegex' ('[':'!':c:cs) =
  case res of
    (Left _) -> res
    (Right res') -> Right $ "[^" ++ c : res'
  where res = charClass cs
globToRegex' ('[':c:cs)     =
  case res of
    (Left _) -> res
    (Right res') -> Right $ "[" ++ c : res'
  where res = charClass cs
globToRegex' ('[':_)        = Left "unterminated character class"

globToRegex' (c:cs)         =
  case res of
    (Left _) -> res
    (Right res') -> Right $ escape c ++ res'
  where res = globToRegex' cs

escape :: Char -> String
escape c | c `elem` regexChars = '\\' : [c]
         | otherwise           = [c]
    where regexChars = "\\+()^$.{}]|"

charClass :: String -> Either GlobError String
charClass (']':cs) =
  case res of
    (Left _) -> res
    (Right res') -> Right $ ']' : res'
  where res = globToRegex' cs
charClass (c:cs)   =
  case res of
    (Left _) -> res
    (Right res') -> Right $ c : res'
  where res = charClass cs
charClass []       = Left "unterminated character class"

matchesGlob :: FilePath -> String -> Bool -> Either GlobError Bool
matchesGlob name pat caseSensitive =
    case res of
      (Right res') -> Right $ name' =~ res'
      (Left e)     -> Left e
    where name' | caseSensitive = name
                | otherwise     = map toLower name
          res = globToRegex pat caseSensitive

stripSlashes :: String -> String
stripSlashes = filter (/= pathSeparator)
