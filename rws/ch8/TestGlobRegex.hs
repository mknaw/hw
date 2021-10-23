import Glob
import GlobRegex

-- main = do
    -- matches <- namesMatching "/Users/mknaw/dev/hw/**.hs"
    -- mapM putStrLn matches

main = do
    putStrLn $ globToRegex "/Users/mknaw/dev/hw/**.hs" True

