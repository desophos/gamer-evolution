module Util where

import           Data.Char                      ( digitToInt )
import           Data.List                      ( nubBy
                                                , sort
                                                )
import qualified Data.Map                      as Map
import qualified Data.Set                      as S
import           GHC.List                       ( foldl1' )
import           Numeric                        ( readInt )
import           Text.Printf                    ( printf )


-- | merges consecutive equal elements.
-- recommended to sort first
merge
    :: Eq a
    => (a -> a -> a) -- ^ combining function
    -> [a] -- ^ list to merge
    -> [a]
merge f xs = mergeF xs []
  where
    mergeF []  acc = reverse acc
    mergeF [x] acc = mergeF [] (x : acc)
    mergeF (x : y : ys) acc =
        if x == y then mergeF ys (f x y : acc) else mergeF (y : ys) (x : acc)

-- | continues merging until there's no more to merge
mergeAll :: Eq a => (a -> a -> a) -> [a] -> [a]
mergeAll f xs = if merged == merge f merged then merged else mergeAll f merged
    where merged = merge f xs

-- | splits a list into multiple equal-length lists
chunk
    :: Int -- ^ the size of the resulting lists
    -> [a] -- ^ the list to split
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

-- | True if the list contains no duplicates
unique :: Eq a => [a] -> Bool
unique []       = True
unique [_     ] = True
unique (x : xs) = x `notElem` xs && unique xs

-- | True if the lists share all elements in any order
sameMatch :: (Foldable t1, Foldable t2, Eq a) => t1 a -> t2 a -> Bool
sameMatch xs ys = all (`elem` xs) ys && all (`elem` ys) xs

-- | returns all pairings (combinations) in a list
matchups2 :: (Eq a) => [a] -> [[a]]
matchups2 xs = nubBy sameMatch [ [x, y] | x <- xs, y <- xs, x /= y ]

-- >>> encodeBcd 4 7
-- "0111"
encodeBcd
    :: Int -- ^ padding size
    -> Int -- ^ decimal to encode
    -> String -- ^ the decimal converted to binary and right-adjusted with zeroes
encodeBcd = printf "%0*b"

-- | given a string of binary digits, returns the number as a decimal Int
decodeBcd :: String -> Int
decodeBcd = fst . head . readInt 2 (`elem` ['0', '1']) digitToInt

-- | apply a list of functions to the same input
-- and combine their outputs
combineWith :: (b -> b -> b) -> [a -> b] -> a -> b
combineWith _       [] _ = error "Util.combineWith requires a list of functions"
combineWith combine fs x = foldl1' combine $ map ($ x) fs

-- | a value x is grouped in the largest bin <= x
-- | if a value is even lower than the first bin,
-- it's grouped in the first bin anyway
-- >>> sortBins [0,10..50] [0,3..59]
-- fromList [(0,[0,3,6,9]),(10,[12,15,18]),(20,[21,24,27]),(30,[30,33,36,39]),(40,[42,45,48]),(50,[51,54,57])]
sortBins
    :: Ord a
    => [a] -- ^ bins to group values into
    -> [a] -- ^ values to group into bins
    -> Map.Map a [a]
sortBins bins = f $ Map.fromSet (const []) (S.fromList bins)
  where
    f acc []       = Map.map sort acc
    f acc (x : xs) = f (Map.adjust (x :) (findBin x) acc) xs
      where
        findBin y = case takeWhile (y >=) bins of
            [] -> head bins
            ys -> last ys
