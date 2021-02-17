module Util where

import           Data.Char
import           Data.List
import           Numeric
import           Text.Printf


-- merges consecutive equal elements
-- using f as the combining function
-- recommended to sort first
merge :: Eq a => (a -> a -> a) -> [a] -> [a]
merge f xs = mergeF xs []
  where
    mergeF []  acc = reverse acc
    mergeF [x] acc = mergeF [] (x : acc)
    mergeF (x : y : ys) acc =
        if x == y then mergeF ys (f x y : acc) else mergeF (y : ys) (x : acc)

-- continues merging until there's no more to merge
mergeAll :: Eq a => (a -> a -> a) -> [a] -> [a]
mergeAll f xs = if merged == merge f merged then merged else mergeAll f merged
    where merged = merge f xs

-- splits xs into multiple `size`-length lists
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk size xs
    | size < 1
    = error "Util.chunk size must be >= 1"
    | size > length xs
    = error "Util.chunk size must be <= list length"
    | length xs `rem` size /= 0
    = error "Util.chunk size must divide list evenly"
    | otherwise
    = y : chunk size ys
    where (y, ys) = splitAt size xs

-- True if xs contains no duplicates
unique :: Eq a => [a] -> Bool
unique []       = True
unique [_     ] = True
unique (x : xs) = x `notElem` xs && unique xs

-- True if its args share all elements in any order
sameMatch :: (Foldable t1, Foldable t2, Eq a) => t1 a -> t2 a -> Bool
sameMatch xs ys = all (`elem` xs) ys && all (`elem` ys) xs

-- returns all pairings (combinations) in a list
matchups2 :: (Eq a) => [a] -> [[a]]
matchups2 xs = nubBy sameMatch [ [x, y] | x <- xs, y <- xs, x /= y ]

-- given padding size and a decimal Int,
-- returns a padded string representation of the number in binary
encodeBcd :: Int -> Int -> String
encodeBcd = printf "%0*b"

-- given a string of binary digits, returns the number as a decimal Int
decodeBcd :: String -> Int
decodeBcd = fst . head . readInt 2 (`elem` ['0', '1']) digitToInt
