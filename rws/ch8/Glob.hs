module Glob (namesMatching) where

import Control.Exception (handle, SomeException)
import Control.Monad (filterM, forM)
import Data.List (isSuffixOf)
import GlobRegex (matchesGlob)
import System.Directory (
      doesDirectoryExist
    , doesFileExist
    , getCurrentDirectory
    , listDirectory
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
          dirs <- getDirs dirName
          let listDir = if isPattern baseName
                        then listMatches
                        else listPlain
          pathNames <- forM dirs $ \dir -> do
                         baseNames <- listDir dir baseName
                         return (map (dir </>) baseNames)
          return $ concat pathNames
      where getDirs dir
              -- TODO this is still bad since it doesn't handle the `/*/foo/**/bar` case
              | "**/" `isSuffixOf` dir = let dir' = take (length dir - 3) dir
                                         in listSubdirsRecursive dir'
              | isPattern dir = namesMatching (dropTrailingPathSeparator dir)
              | otherwise     = return [dir]

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
    names <- listDirectory dirName'
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

listSubdirsRecursive :: String -> IO [String]
listSubdirsRecursive dirName = do
    contents <- listDirectory dirName
    subDirs <- filterM dirFilter contents
    let subDirs' = filter (not . isHidden) subDirs
    subContents <- mapM (\d -> listSubdirsRecursive $ dirName </> d) subDirs'
    return $ dirName : concat subContents
  where dirFilter d = doesDirectoryExist (dirName </> d)
