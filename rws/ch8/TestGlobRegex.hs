import Control.Monad (filterM)
import Data.Either (
      isLeft
    , lefts
    , rights
    )
import System.Directory (
      doesDirectoryExist
    , doesFileExist
    , getCurrentDirectory
    , listDirectory
    )
import System.FilePath ((</>))

import Glob
import GlobRegex

main = do
    -- matches <- namesMatching "/Users/mknaw/dev/hw/rws/*/*.hs"
    matches <- namesMatching "/Users/mknaw/dev/*/**/*.hs"
    case matches of
      (Left e) -> putStrLn e
      (Right matches') -> mapM_ putStrLn matches'

