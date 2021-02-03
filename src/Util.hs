module Util where

import Numeric
import Data.List
import Data.Char
import Text.Printf

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk size xs =
    let (y, ys) = splitAt size xs
    in y : chunk size ys

-- True if its args share all elements in any order
sameMatch :: (Foldable t1, Foldable t2, Eq a) => t1 a -> t2 a -> Bool
sameMatch xs = all (`elem` xs)

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
