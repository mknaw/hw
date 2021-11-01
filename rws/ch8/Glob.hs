module Glob (namesMatching) where

import Control.Exception (handle, SomeException)
import Control.Monad (
      filterM
    , forM
    , sequence
    )
import Data.Either (
      isLeft
    , isRight
    , rights
    )
import Data.List (isSuffixOf)
import GlobRegex (
      GlobError
    , matchesGlob
    )
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

allRight :: Foldable f => f (Either a b) -> Bool
allRight = all isRight

namesMatching :: String -> IO (Either GlobError [String])
namesMatching pat
  | not (isPattern pat) = do
      exists <- doesNameExist pat
      return $ Right [pat | exists]
  | otherwise = do
      case splitFileName pat of
        ("", baseName) -> do
          curDir <- getCurrentDirectory
          listMatches curDir baseName
        (dirName, "**") -> do
          -- It's not really "dirs," but this is from the textbook..
          dirs <- getDirs dirName
          case dirs of
            (Left e) -> return $ Left e
            (Right dirs') -> do
              dirs' <- filterM doesDirectoryExist dirs'
              subDirs <- mapM listSubdirsRecursive dirs'
              return $ Right (concat subDirs)
        (dirName, baseName) -> do
          dirs <- getDirs dirName
          let listDir = if isPattern baseName
                        then listMatches
                        else listPlain
          case dirs of
            (Left e) -> return $ Left e
            (Right dirs') -> do
              pathNames <- forM dirs' $ \dir -> do
                             baseNames <- listDir dir baseName
                             case baseNames of
                               (Left e) -> return $ Left e
                               (Right baseNames') -> return $ Right (map (dir </>) baseNames')
              if any isLeft pathNames
                 then return $ Left "uhoh"
                 else return $ Right (concat (rights pathNames))
      where getDirs dir
              | isPattern dir = namesMatching (dropTrailingPathSeparator dir)
              | otherwise     = return $ Right [dir]

doesNameExist :: FilePath -> IO Bool
doesNameExist name = do
  handle handler $ do
    getFileStatus name
    return True
  where handler :: SomeException -> IO Bool
        handler _ = return False

listMatches :: FilePath -> String -> IO (Either GlobError [String])
listMatches dirName pat = do
  dirName' <- if null dirName
              then getCurrentDirectory
              else return dirName
  handle handler $ do
    names <- listDirectory dirName'
    let names' = if isHidden pat
                 then filter isHidden names
                 else filter (not . isHidden) names
    return $ eitherFilter filterF names'
  where handler :: SomeException -> IO (Either GlobError [String])
        handler _ = return (Right [])
        filterF :: FilePath -> Either GlobError Bool
        filterF fp = matchesGlob fp pat isUnix

eitherFilter :: (a -> Either b Bool) -> [a] -> Either b [a]
eitherFilter _ [] = Right []
eitherFilter f (x:xs) =
  case f x of
    (Right True) -> 
      case next of
        (Right xs') -> Right $ x : xs'
        (Left e) -> Left e
      where next = eitherFilter f xs
    (Right False) -> eitherFilter f xs
    (Left e) -> Left e


isHidden ('.':_) = True
isHidden _       = False

listPlain :: FilePath -> String -> IO (Either GlobError [String])
listPlain dirName baseName = do
  exists <- if null baseName
            then doesDirectoryExist dirName
            else doesNameExist (dirName </> baseName)
  return $ Right [baseName | exists]

listSubdirsRecursive :: String -> IO [String]
listSubdirsRecursive dirName = do
    contents <- listDirectory dirName
    subDirs <- filterM dirFilter contents
    let subDirs' = filter (not . isHidden) subDirs
    subContents <- mapM (\d -> listSubdirsRecursive $ dirName </> d) subDirs'
    return $ dirName : concat subContents
  where dirFilter d = doesDirectoryExist (dirName </> d)
