import Data.List (break, dropWhile)

globMatch :: String -> FilePath -> Bool
globMatch [] [] = True
globMatch pat path
  | pat == path = True
globMatch [] (_:_) = False
globMatch (_:_) [] = False
globMatch (p:ps) (f:fs)
  | p == '*' =
      case ps of
        ['*']     -> error "didn't implement this"
        ('*':ps') -> any ((== True) . globMatch ps') (allSubPaths fs)
        _   -> globMatch ps (dropWhile (/= '/') fs)
  | p == '[' =
    let charSet = getCharSet ps
        criterion = 
          if head ps == '^'
            then not . (`elem` charSet)
            else (`elem` charSet)
    in globMatch ps (dropWhile criterion fs)
  | otherwise = p == f && globMatch ps fs

getCharSet :: String -> String
getCharSet chars =
  if ']' `elem` chars
     then takeWhile (/= ']') chars
     else error "invalid pattern"

-- TODO Actually not quite right - should return a list of all
-- the sep dropped parts.
dropUntilLastSep :: String -> String
dropUntilLastSep fp =
  let (start, end) = break (== '/') fp
  in case start of
       [] -> end
       _  -> dropUntilLastSep $ tail end

allSubPaths :: String -> [String]
allSubPaths fp =
  case fp of
    []         -> []
    ('/':rest) -> fp : allSubPaths (dropWhile (/= '/') rest)
    rest       -> allSubPaths (dropWhile (/= '/') rest)

main = do
  let pat = "/Users/mknaw/**/rws/"
  let path = "/Users/mknaw/dev/hw/rws/"
  print $ globMatch pat path
