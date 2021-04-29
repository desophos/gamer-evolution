{-# LANGUAGE FlexibleContexts #-}
module Util where

import           Data.ByteString.Builder        ( Builder
                                                , intDec
                                                )
import qualified Data.ByteString.Lazy          as B
import qualified Data.ByteString.Lazy.Char8    as C
import           Data.Char                      ( digitToInt )
import           Data.Digits                    ( digits
                                                , unDigits
                                                )
import           Data.List                      ( sort
                                                , sortOn
                                                , tails
                                                )
import qualified Data.Map.Strict               as Map
import           Data.MonoTraversable           ( Element
                                                , MonoFoldable(olength, onull)
                                                )
import qualified Data.Set                      as S
import qualified Data.Vector                   as V
import           GHC.List                       ( foldl1' )

class MonoFoldable a => Chunkable a where
    bottom :: a
    chunkAt :: Int -> a -> (a, a)
    -- | Splits a Chunkable into multiple equal-length chunks.
    chunk
        :: Int -- ^ The size of the resulting chunks.
        -> a -- ^ The Chunkable to split.
        -> [a]
    chunk size xs
        | onull xs
        = bottom
        | size < 1
        = error $ "Util.chunk size must be >= 1" ++ errSuffix
        | size > olength xs
        = error $ "Util.chunk size must be <= length" ++ errSuffix
        | olength xs `rem` size /= 0
        = error $ "Util.chunk size must divide length evenly" ++ errSuffix
        | otherwise
        = y : chunk size ys
      where
        (y, ys)   = chunkAt size xs
        errSuffix = ". size = " ++ show size ++ "; length = " ++ show (olength xs)

instance Chunkable [a] where
    bottom  = []
    chunkAt = splitAt
instance Chunkable B.ByteString where
    bottom  = B.empty
    chunkAt = B.splitAt . fromIntegral

class MonoFilterable mono where
    ofilter :: (Element mono -> Bool) -> mono -> mono

instance MonoFilterable [a] where
    ofilter = filter
instance MonoFilterable B.ByteString where
    ofilter = B.filter

omit
    :: (MonoFilterable mono, Eq (Element mono))
    => mono -- ^ Filterable to omit from.
    -> Element mono -- ^ Element to omit.
    -> mono -- ^ Filterable without the omitted element.
omit xs x = ofilter (/= x) xs

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

-- | True if the list contains no duplicates.
unique :: Ord a => [a] -> Bool
unique xs = f (sort xs)
  where
    f []           = True
    f [_         ] = True
    f (x : y : ys) = x /= y && f (y : ys)

-- | True if the lists share all elements in any order.
-- Duplicate elements are combined.
sameMatch :: Ord a => [a] -> [a] -> Bool
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
    :: Int -- ^ Combination length (`1 < k <= size items`).
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

-- | Returns all @k@-length combinations between @items@ 
-- and the first @nTake@ elements of @items@ sorted by @f@.
--
-- >>> import qualified Data.Set as Set
-- >>> import Data.Ord ( Down(Down) )
-- >>> matchWithBest 3 2 Down (Set.fromList ['a'..'c'])
-- ["ccc","ccb","cbc","cbb","bcc","bcb","bbc","bbb","acc","acb","abc","abb"]

-- See 'matchups' for implementation details.
matchWithBest
    :: Ord b
    => Int -- ^ Number of items per match (@k@).
    -> Int -- ^ Number of elements to take from sorted @items@ (@nTake@).
    -> (a -> b) -- ^ Key fn to sort @items@ by before taking @nTake@ elements (@f@).
    -> S.Set a -- ^ Set to generate matches from (@items@).
    -> [[a]]
matchWithBest k nTake f items
    | k < 1 =  error
    $  "Util.matchWithBest match length must be >= 1. k = "
    ++ show k
    | otherwise = map (map (itemsV V.!)) matchIndexes
  where
    itemsV       = V.fromList . sortOn f . S.toAscList $ items
    matchIndexes = (memoize . memoize . memoize)
        (\i n k ->
            sequence $ [0 .. i - 1] : replicate (k - 1) [0 .. min i n - 1]
        )
        (length itemsV)
        nTake
        k

(<<) :: Monad m => m a -> m b -> m a
(<<) = flip (>>)

-- | https://bor0.wordpress.com/2020/12/11/haskell-memoization-and-evaluation-model/
memoize :: (Int -> a) -> (Int -> a)
memoize f = (map f [0 ..] !!)

-- | Memoized over both parameters for better performance over many calls.
-- >>> import Data.ByteString.Builder (toLazyByteString)
-- >>> toLazyByteString $ encodeBcd 4 7
-- "0111"
encodeBcd
    :: Int -- ^ padding size
    -> Int -- ^ decimal to encode
    -> Builder -- ^ the decimal converted to binary and right-adjusted with zeroes
encodeBcd = (memoize . memoize)
    (\width x ->
        let encoded = digits 2 x
            padding = replicate (max 0 (width - length encoded)) 0
            bsify   = mconcat . map intDec
        in  bsify padding <> bsify encoded
    )

-- | Given a string of binary digits, returns the number as a decimal Int.
decodeBcd :: B.ByteString -> Int
decodeBcd = unDigits 2 . map digitToInt . C.unpack

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
