module GlobRegex
    (
      globToRegex
    , matchesGlob
    ) where

import Data.Char (toLower)
import Text.Regex.Posix ((=~))
import System.FilePath (pathSeparator)

globToRegex :: String -> Bool -> String
globToRegex cs caseSensitive = '^' : globToRegex' cs' ++ "$"
    where cs' = if caseSensitive then cs else map toLower cs

globToRegex' :: String -> String
globToRegex' ""             = ""

globToRegex' ('*':'*':cs)       = ".*" ++ (globToRegex' . stripSlashes) cs
globToRegex' ('*':cs)       = ".*" ++ globToRegex' cs
globToRegex' ('?':cs)       = '.' : globToRegex' cs

globToRegex' ('[':'!':c:cs) = "[^" ++ c : charClass cs
globToRegex' ('[':c:cs)     = "[" ++ c : charClass cs
globToRegex' ('[':_)        = error "unterminated character class"

globToRegex' (c:cs)         = escape c ++ globToRegex' cs

escape :: Char -> String
escape c | c `elem` regexChars = '\\' : [c]
         | otherwise           = [c]
    where regexChars = "\\+()^$.{}]|"

charClass :: String -> String
charClass (']':cs) = ']' : globToRegex' cs
charClass (c:cs)   = c : charClass cs
charClass []       = error "unterminated character class"

matchesGlob :: FilePath -> String -> Bool -> Bool
matchesGlob name pat caseSensitive = name' =~ globToRegex pat caseSensitive
    where name' | caseSensitive = name
                | otherwise     = map toLower name

stripSlashes :: String -> String
stripSlashes = filter (/= pathSeparator)
