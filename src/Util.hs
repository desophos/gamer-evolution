module Util where

import Numeric
import Data.List
import Data.Char
import Text.Printf


-- merges consecutive equal elements
-- using f as the combining function
-- recommended to sort first
-- n.b. if (f x y) == (head xs), then they will be merged together
merge :: Eq a => (a -> a -> a) -> [a] -> [a]
merge _ [] = []
merge _ [x] = [x]
merge f (x:y:xs) =
    if x == y
    then merge f (f x y : xs)
    else x : merge f (y : xs)

-- continues merging until there's no more to merge
mergeAll :: Eq a => (a -> a -> a) -> [a] -> [a]
mergeAll f xs
    | merged == merge f merged = merged
    | otherwise = mergeAll f merged
    where merged = merge f xs

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk size xs =
    let (y, ys) = splitAt size xs
    in y : chunk size ys

-- True if its args share all elements in any order
sameMatch :: (Foldable t1, Foldable t2, Eq a) => t1 a -> t2 a -> Bool
sameMatch xs ys = all (`elem` xs) ys && all (`elem` ys) xs

-- returns all pairings (combinations) in a list
matchups2 :: (Eq a) => [a] -> [[a]]
matchups2 xs = nubBy sameMatch [[x,y] | x <- xs, y <- xs, x /= y]

-- given padding size and a decimal Int,
-- returns a padded string representation of the number in binary
encodeBcd :: Int -> Int -> String
encodeBcd = printf "%0*b"

-- given a string of binary digits, returns the number as a decimal Int
decodeBcd :: String -> Int
decodeBcd = fst . head . readInt 2 (`elem` ['0','1']) digitToInt
