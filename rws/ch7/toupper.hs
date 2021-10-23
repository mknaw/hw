import Data.Char(toUpper)

main = interact (map toUpper . (++) "your data:\n")
