module Glob (namesMatching) where

import Control.Exception (handle, SomeException)
import Control.Monad (forM)
import GlobRegex (matchesGlob)
import System.Directory (
      doesDirectoryExist
    , doesFileExist
    , getCurrentDirectory
    , getDirectoryContents
    )
import System.FilePath (
      dropTrailingPathSeparator
    , pathSeparator
    , splitFileName
    , (</>)
    )
import System.Posix.Files (getFileStatus)

isUnix :: Bool
isUnix = pathSeparator == '/'

isPattern :: String -> Bool
isPattern = any (`elem` "[*?")

namesMatching :: String -> IO [String]
namesMatching pat
  | not (isPattern pat) = do
      exists <- doesNameExist pat
      return [pat | exists]
  | otherwise = do
      case splitFileName pat of
        ("", baseName) -> do
          curDir <- getCurrentDirectory
          listMatches curDir baseName
        (dirName, baseName) -> do
          dirs <- if isPattern dirName
                  then namesMatching (dropTrailingPathSeparator dirName)
                  else return [dirName]
          let listDir = if isPattern baseName
                        then listMatches
                        else listPlain
          pathNames <- forM dirs $ \dir -> do
                         baseNames <- listDir dir baseName
                         return (map (dir </>) baseNames)
          return (concat pathNames)

doesNameExist :: FilePath -> IO Bool
doesNameExist name = do
  handle handler $ do
    getFileStatus name
    return True
  where handler :: SomeException -> IO Bool
        handler _ = return False

listMatches :: FilePath -> String -> IO [String]
listMatches dirName pat = do
  dirName' <- if null dirName
              then getCurrentDirectory
              else return dirName
  handle handler $ do
    names <- getDirectoryContents dirName'
    let names' = if isHidden pat
                 then filter isHidden names
                 else filter (not . isHidden) names
    return (filter (\fp -> matchesGlob fp pat isUnix) names')
  where handler :: SomeException -> IO [String]
        handler _ = return []

isHidden ('.':_) = True
isHidden _       = False

listPlain :: FilePath -> String -> IO [String]
listPlain dirName baseName = do
  exists <- if null baseName
            then doesDirectoryExist dirName
            else doesNameExist (dirName </> baseName)
  return [baseName | exists]
