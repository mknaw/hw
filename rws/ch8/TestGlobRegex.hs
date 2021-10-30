import Control.Monad (filterM)
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
    matches <- namesMatching "/Users/mknaw/dev/hw/**/*.hs"
    mapM putStrLn matches

