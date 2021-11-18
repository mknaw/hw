import Prelude hiding (traverse)

import Control.Exception (handle, SomeException(..))
import Control.Monad (forM)
import Data.Time.Clock (UTCTime(..))
import System.Directory (
    doesDirectoryExist
  , getDirectoryContents
  , getModificationTime
  , getPermissions
  , Permissions(..)
  )
import System.FilePath ((</>))
import System.IO (
    IOMode(..)
  , hFileSize
  , withFile
  )

data Info = Info {
    infoPath :: FilePath
  , infoPerms :: Maybe Permissions
  , infoSize :: Maybe Integer
  , infoModTime :: Maybe UTCTime
  } deriving (Eq, Ord, Show)

traverse :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverse order path = do
  names <- getUsefulContents path
  contents <- mapM getInfo (path : map (path </>) names)
  fmap concat $ forM (order contents) $ \info -> do
    if isDirectory info && infoPath info /= path
     then traverse order (infoPath info)
     else return [info]

getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
  names <- getDirectoryContents path
  return $ filter (`notElem` [".", ".."]) names

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms

maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle (\(SomeException _) -> return Nothing) (Just `fmap` act)

getInfo :: FilePath -> IO Info
getInfo path = do
  perms <- maybeIO (getPermissions path)
  size <- maybeIO (withFile path ReadMode hFileSize)
  modified <- maybeIO (getModificationTime path)
  return (Info path perms size modified)
