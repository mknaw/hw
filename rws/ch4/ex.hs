import Data.Maybe
import Data.Char (isDigit, ord)
import Data.List (foldl')

-- 1. Write your own "safe" definitions of the standard partial list functions,
--    but make sure that yours never fail.

safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead _     = Nothing

safeTail :: [a] -> Maybe [a]
safeTail (_:xs) = Just xs
safeTail _      = Nothing

safeLast :: [a] -> Maybe a
safeLast (x:xs) = case xs of
    [] -> Just x
    _  -> safeLast xs
safeLast _ = Nothing

safeInit :: [a] -> Maybe [a]
safeInit (x:xs) = case xs of
    [_] -> Just [x]
    _  -> Just (x : fromMaybe [] (safeInit xs))
safeInit _ = Nothing

-- 2. Write a function splitWith that acts similarly to words, but takes a predicate and
--    a list of any type, and splits its input list on every element for which the predicate
--    returns False.

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith f lst = case pre of
        [] -> [suf]
        _  -> case suf of
                [] -> [pre]
                _  -> pre : splitWith f (tail suf)
    where tup = break f lst
          pre = fst tup
          suf = snd tup

-- 1. Use a fold to write an asInt fn.

asInt :: [Char] -> Int
asInt []  = error "bad input" 
asInt "-" = error "bad input" 
asInt s   = negate' (foldl' step 0 s)
    where step a b
            | b == '-'  = a
            | isDigit b = 10 * a + (ord b - ord '0')
            | otherwise = error "bad input"
          negate' = if head s == '-' then negate else id

-- 2. Rewrite asInt to not call `error`.

newtype ErrorMessage = ErrorMessage String
    deriving (Show)

asIntEither :: String -> Either ErrorMessage Int
asIntEither ""       = Left (ErrorMessage "bad")
asIntEither ('-':xs) = case asIntEither xs of
    Right d -> Right (-1 * d)
    Left e -> Left e
asIntEither l        = foldl' step (Right 0) l
    where step (Right a) b
            | isDigit b = Right (10 * a + (ord b - ord '0'))
            | otherwise = Left (ErrorMessage "bad")
          step (Left a) b = Left a

-- 3. Write your own definition of `concat` using `foldr`.

concat :: [[a]] -> [a]
concat = foldr (++) []

-- 4. Write a version of takeWhile with standard recursion and one with foldr.

takeWhileRecur :: (a -> Bool) -> [a] -> [a]
takeWhileRecur f = takeWhileRecurLoop f []

takeWhileRecurLoop f acc [] = acc
takeWhileRecurLoop f acc (x:xs) = takeWhileRecurLoop f acc' xs
    where acc' = if f x then acc ++ [x] else acc

takeWhileFoldr :: (a -> Bool) -> [a] -> [a]
takeWhileFoldr f = foldr step []
    where step b a
            | f b = b : a
            | otherwise = []

-- 5. Write your own implementation of groupBy using a fold.

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy f = foldr step []
    where step b a
            | null a = [[b]]
            | f b (head (head a)) = (b : head a) : tail a
            | otherwise = [b] : a

-- 6. How many of the following can you rewrite using list folds?

anyFoldl :: (a -> Bool) -> [a] -> Bool
anyFoldl f = foldl' step False
    where step a b
            | a         = True
            | f b       = True
            | otherwise = False

-- cycle - don't think folds are a good use here...?
cycleFoldr :: [a] -> [a]
cycleFoldr lst = foldr step lst lst
    -- very slow
    where step b a = b : cycleFoldr a

wordsFoldr :: String -> [String]
wordsFoldr (' ':rest) = wordsFoldr rest
wordsFoldr s          = foldr step [] s
    where step b (word:rest)
            | b == ' '  && null word = [] : rest
            | b == ' '               = [] : word : rest
            | otherwise              = (b : word) : rest
          step ' ' [] = []
          step b [] = [[b]]

unlinesFoldl :: [String] -> String
unlinesFoldl = foldl' step ""
    where step a b = a ++ b ++ "\n"
