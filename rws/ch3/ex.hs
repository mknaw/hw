import Data.List (minimumBy, sortBy)

data List a = Cons a (List a)
              | Nil
              deriving (Show)

-- 1. Write a function that takes `List a` and converts to `[a]`.

fromList :: List a -> [a]
fromList (Cons x a) = x : fromList a
fromList Nil = []

-- 2. Define a tree type that has only one constructor.
--    Instead of the Empty constructor, use the Maybe type to refer to a node's children.

data Tree2 a = Node2 a (Maybe (Tree2 a)) (Maybe (Tree2 a))
               deriving (Show)

-- 1. Write a function that computes the number of elements in a list.
-- 2. Add a type signature for your function to your source file.

numberOfElements :: [a] -> Int
numberOfElements = foldr (\x -> (+) 1) 0

-- 3. Write a function that computes the mean of a list.

mean lst = fromIntegral (sum lst) / fromIntegral (length lst)

-- 4. Turn a list into a palindrome - e.g. [1, 2, 3] -> [1, 2, 3, 3, 2, 1].

myReverse (x:xs) = myReverse xs ++ [x]
myReverse [] = []

makePalindrome xs = xs ++ myReverse xs

-- 5. Write a function to determine whether a list is a palindrome

-- feel like this is probably one of the least efficient ways...
isPalindrome xs = xs ++ myReverse xs == myReverse xs ++ xs

-- 6. Create a function that sorts a list of lists based on the length of each sublist.

sortFn :: [a] -> [a] -> Ordering
sortFn l1 l2
    | length l1 > length l2 = GT
    | length l1 < length l2 = LT
sortFn _ _ = EQ

listSorter = Data.List.sortBy sortFn

-- 7. Create a function that joins a list of lists together using a separator value.

intersperse :: a -> [[a]] -> [a]
intersperse sep [x] = x
intersperse sep (x:xs) = x ++ sep : intersperse sep xs
intersperse sep [] = []

-- 8. Write a function to find the height of a `Tree`.

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

treeHeight tree =
    case tree of
        Node _ l r -> 1 + max (treeHeight l) (treeHeight r)
        Empty -> 0

-- 9. Consider three two-dimensional points a, b, and c.
--    If we look at the angle formed by the line segment from a to b and the line
--    segment from b to c, it either turns left, turns right, or forms a straight line.
--    Define a Direction data type that lets you represent these possibilities.

data TurnDirection = LeftTurn
                     | RightTurn
                     | Straight
                     deriving (Show)

-- 10. Write a function that calculates the turn made by three 2D points and
--     returns a Direction.

data Point = Point {
      x :: Double
    , y :: Double
} deriving (Show, Eq)

accountForDirection a b
    | x a < x b = id
    | x a > x b = not
    | y a < y b = id
    | otherwise = not

slope :: Point -> Point -> Double
slope a b = (y b - y a) / (x b - x a)

intercept :: Point -> Point -> Double
intercept a b = y a - slope a b * x a

-- I guess there's a simpler way - `(x2 - x1)(y3 - y1) - (y2 - y1)(x3 - x1)`
classifyTurn :: Point -> Point -> Point -> TurnDirection
classifyTurn a b c
    | isInfinite m && x0 == x a = Straight
    | isInfinite m && direction (x0 < x a) = LeftTurn
    | isInfinite m = LeftTurn
    | y0 == m * x0 + i = Straight
    | direction (y0 > m * x0 + i) = LeftTurn
    | otherwise = RightTurn
    where direction = accountForDirection a b
          m         = slope a b
          i         = intercept a b
          x0        = x c
          y0        = y c

-- 11. Define a function that takes a list of 2D points and computes the direction of
--     each successive triple.

classifyTurns :: [Point] -> [TurnDirection]
classifyTurns (a:b:c:xs) = classifyTurn a b c : classifyTurns (b:c:xs)
classifyTurns _ = []

-- 12. Implement Graham scan algorithm for finding the convex hull of a set of 2D points.

firstOrdering :: Point -> Point -> Ordering
firstOrdering a b
    | y a > y b = GT
    | y a < y b = LT
    | otherwise = compare (x a) (x b)

firstPoint :: [Point] -> Point
firstPoint = minimumBy firstOrdering

slopeOrdering p a b
    | slope p a < 0 && slope p b >= 0 = GT
    | slope p a >= 0 && slope p b < 0 = LT
    | otherwise = compare (slope p a) (slope p b)

removeFromList _ [] = []
removeFromList x (y:ys)
    | x == y    = removeFromList x ys
    | otherwise = y : removeFromList x ys

grahamScan :: [Point] -> [Point]
grahamScan lst = p : keeperFn sorted turns ++ [p]
    where p      = firstPoint lst
          sorted = sortBy (slopeOrdering p) (removeFromList p lst)
          turns  = classifyTurns (p : sorted ++ [p])

keeperFn :: [a] -> [TurnDirection] -> [a]
keeperFn (x:xs) (t:ts) = case t of
    RightTurn -> keeperFn xs ts
    _         -> x : keeperFn xs ts
keeperFn [] [] = []
keeperFn _ _ = error "should not happen"

