import System.Environment (getArgs)

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input, output] -> interactWith function input output
            _               -> putStrLn "error: exactly two arguments needed"
        myFunction = fixLines

fixLines :: String -> String
fixLines input = unlines (transpose (splitLines input))

splitLines :: String -> [String]
splitLines [] = []
splitLines cs =
  let (pre, suf) = break isLineTerminator cs
  in pre : case suf of
             ('\r':'\n':rest) -> splitLines rest
             ('\r':rest)      -> splitLines rest
             ('\n':rest)      -> splitLines rest
             _                -> []

isLineTerminator c = c == '\r' || c == '\n'

splitAts = map (splitAt 1)
fsts = map fst
snds = map snd

transpose :: [String] -> [String]
transpose lst =
    if all (== "") secondBits then [concat firstBits]
    else concat firstBits : transpose (snds split)
    where split = splitAts lst
          firstBits = fsts split
          secondBits = snds split
