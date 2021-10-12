import Data.Maybe

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
