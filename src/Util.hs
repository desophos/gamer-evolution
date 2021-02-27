module Util where

import           Data.Char                      ( digitToInt )
import           Data.List                      ( sort
                                                , tails
                                                )
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as S
import qualified Data.Vector                   as V
import           GHC.List                       ( foldl1' )
import           Numeric                        ( readInt )
import           Text.Printf                    ( printf )


omit
    :: Eq a
    => [a] -- ^ List to omit from.
    -> a -- ^ Element to omit.
    -> [a] -- ^ List without the omitted element.
omit xs x = filter (/= x) xs

-- | Merges consecutive equal elements.
-- Recommended to sort first.
merge
    :: Eq a
    => (a -> a -> a) -- ^ Combining function.
    -> [a] -- ^ List to merge.
    -> [a]
merge f xs = mergeF xs []
  where
    mergeF []  acc = reverse acc
    mergeF [x] acc = mergeF [] (x : acc)
    mergeF (x : y : ys) acc =
        if x == y then mergeF ys (f x y : acc) else mergeF (y : ys) (x : acc)

-- | Continues merging until there's no more to merge.
mergeAll :: Eq a => (a -> a -> a) -> [a] -> [a]
mergeAll f xs = if merged == merge f merged then merged else mergeAll f merged
    where merged = merge f xs

-- | Splits a list into multiple equal-length lists.
chunk
    :: Int -- ^ The size of the resulting lists.
    -> [a] -- ^ The list to split.
    -> [[a]]
chunk _ [] = []
chunk size xs
    | size < 1
    = error $ "Util.chunk size must be >= 1" ++ errSuffix
    | size > length xs
    = error $ "Util.chunk size must be <= list length" ++ errSuffix
    | length xs `rem` size /= 0
    = error $ "Util.chunk size must divide list evenly" ++ errSuffix
    | otherwise
    = y : chunk size ys
  where
    (y, ys)   = splitAt size xs
    errSuffix = ". size = " ++ show size ++ "; length = " ++ show (length xs)

-- | True if the list contains no duplicates.
unique :: Eq a => [a] -> Bool
unique []       = True
unique [_     ] = True
unique (x : xs) = x `notElem` xs && unique xs

-- | True if the lists share all elements in any order.
-- Duplicate elements are combined.
sameMatch :: (Ord a) => [a] -> [a] -> Bool
sameMatch xs ys = S.fromList xs == S.fromList ys

-- | Returns `[]` if `n > length items`.
-- | Complexity of repeated calls with the same `n` and `k`: 
-- `O( n! / k!(n-k)! )` where `n = size items`
-- | Adapted from https://rosettacode.org/wiki/Combinations#Haskell.

-- >>> matchups 3 $ S.fromDistinctAscList ['a'..'e']
-- ["abc","abd","abe","acd","ace","ade","bcd","bce","bde","cde"]

-- Data.Vector.! is O(1), so converting `items` to Vector allows us 
-- to memoize the expensive work of finding combinations
-- by calculating the combination indexes once per (n, length itemsV) pair
-- and using those indexes to generate the combinations of items.
matchups
    :: Ord a
    => Int -- ^ Combination length (`1 < k <= size items`).
    -> S.Set a -- ^ Set to find combinations in (`items`).
    -> [[a]] -- ^ All unique length-n combinations in `items`.
matchups k items = if k < 0
    then error $ "Util.matchups k must be >= 0. k = " ++ show k
    else map (map (itemsV V.!)) combIndexes
  where
    itemsV = V.fromList . S.toAscList $ items
    combIndexes =
        (memoize . memoize) (\x -> comb [0 .. x - 1]) (length itemsV) k
    comb _  0 = [[]]
    comb xs m = [ y : zs | y : ys <- tails xs, zs <- comb ys (m - 1) ]

(<<) :: Monad m => m a -> m b -> m a
(<<) = flip (>>)

-- | https://bor0.wordpress.com/2020/12/11/haskell-memoization-and-evaluation-model/
memoize :: (Int -> a) -> (Int -> a)
memoize f = (map f [0 ..] !!)

-- | Memoized over both parameters for better performance over many calls.
-- >>> encodeBcd 4 7
-- "0111"
encodeBcd
    :: Int -- ^ padding size
    -> Int -- ^ decimal to encode
    -> String -- ^ the decimal converted to binary and right-adjusted with zeroes
encodeBcd = memoize . memoize $ printf "%0*b"

-- | Given a string of binary digits, returns the number as a decimal Int.
decodeBcd :: String -> Int
decodeBcd = fst . head . readInt 2 (`elem` ['0', '1']) digitToInt

-- | Apply a list of functions to the same input
-- and combine their outputs.
combineWith :: (b -> b -> b) -> [a -> b] -> a -> b
combineWith _       [] _ = error "Util.combineWith requires a list of functions"
combineWith combine fs x = foldl1' combine $ map ($ x) fs

-- | A value x is grouped in the largest bin <= x.
-- | If a value is even lower than the first bin,
-- it's grouped in the first bin anyway.
-- >>> sortBins [0,10..50] [0,3..59]
-- fromList [(0,[0,3,6,9]),(10,[12,15,18]),(20,[21,24,27]),(30,[30,33,36,39]),(40,[42,45,48]),(50,[51,54,57])]
sortBins
    :: Ord a
    => [a] -- ^ Bins to group values into.
    -> [a] -- ^ Values to group into bins.
    -> Map.Map a [a]
sortBins bins = f $ Map.fromSet (const []) (S.fromList bins)
  where
    f acc []       = Map.map sort acc
    f acc (x : xs) = f (Map.adjust (x :) (findBin x) acc) xs
      where
        findBin y = case takeWhile (y >=) bins of
            [] -> head bins
            ys -> last ys
