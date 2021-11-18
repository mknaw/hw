import Control.Monad (filterM)
import System.Directory (
    Permissions(..)
  , getModificationTime
  , getPermissions
  )
import Data.Time.Clock (UTCTime(..))
import System.FilePath (takeExtension)
import Control.Exception (bracket, handle, SomeException(..))
import System.IO (
    IOMode(..)
  , hClose
  , hFileSize
  , openFile
  , withFile
  )

import RecursiveContents (getRecursiveContents)

type Predicate =  FilePath
               -> Permissions
               -> Maybe Integer
               -> UTCTime
               -> Bool

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle (\(SomeException _) -> return Nothing) $
  withFile path ReadMode $ \h -> do
    size <- hFileSize h
    return $ Just size

-- Exercise - is the order in which we call bracket and handle important?
-- I don't... think so? Or maybe it does for handling failures in acquiring handle...
getFileSize' :: FilePath -> IO (Maybe Integer)
getFileSize' path = withFile path ReadMode $ \h -> do
  handle (\(SomeException _) -> return Nothing) $ do
    size <- hFileSize h
    return (Just size)

betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind p path = getRecursiveContents path >>= filterM check
  where check name = do
          perms <- getPermissions name
          size <- getFileSize name
          modified <- getModificationTime name
          return (p name perms size modified)

myTest path _ (Just size) _ =
  takeExtension path == ".cpp" && size > 131072
myTest _ _ _ _ = False

type InfoP a =  FilePath
             -> Permissions
             -> Maybe Integer
             -> UTCTime
             -> a

pathP :: InfoP FilePath
pathP path _ _ _ = path

sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing     _ = -1

(==?) :: (Eq a) => InfoP a -> a -> InfoP Bool
(==?) f k w x y z = f w x y z == k
infix 4 ==?

liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP q f k w x y z = f w x y z `q` k

(>?), (<?) :: (Ord a) => InfoP a -> a -> InfoP Bool
(>?) = liftP (>)
(<?) = liftP (<)
infix 4 >?, <?

liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 q f g w x y z = f w x y z `q` g w x y z

(&&?) = liftP2 (&&)
(||?) = liftP2 (||)
infix 3 &&?, ||?

liftPath :: (FilePath -> a) -> InfoP a
liftPath f w _ _ _ = f w

myTest2 = liftPath takeExtension ==? ".cpp" &&?  sizeP >? 131072
