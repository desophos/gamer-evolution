{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
module TestUtil where

import qualified Data.Set as Set
import Data.List
import Test.QuickCheck
import Test.QuickCheck.All()
import Test.Invariant
import Util


newtype ChunkArgs a = ChunkArgs (Int, [a]) deriving (Eq, Show)

instance Arbitrary a => Arbitrary (ChunkArgs a) where
    arbitrary = do
        xs <- listOf1 (arbitrary :: Gen a)
        let len = length xs
        n <- suchThat (arbitrary :: Gen Int)
                      (\y -> y > 0 && y <= len && len `rem` y == 0)
        return $ ChunkArgs (n, xs)


prop_mergeAllNeighbors :: (Eq a, Arbitrary a, CoArbitrary a) => [a] -> Gen Bool
prop_mergeAllNeighbors xs = do
    let eqNeighbor [] = False
        eqNeighbor [_] = False
        eqNeighbor (x:y:ys) = x == y || eqNeighbor (y:ys)
    f <- arbitrary
    return . not . eqNeighbor $ mergeAll f xs

prop_chunkConcats :: Eq a => ChunkArgs a -> Bool
prop_chunkConcats args = concat `inverts` chunk n $ xs
    where ChunkArgs (n, xs) = args

prop_chunkUniform :: ChunkArgs a -> Bool
prop_chunkUniform args = all ((== n) . length) (chunk n xs)
    where ChunkArgs (n, xs) = args

prop_unique :: Ord a => [a] -> Bool
prop_unique xs = (unique xs && setEq xs) || not (unique xs || setEq xs)
    where setEq ys = sort ys == (Set.toList . Set.fromList) ys

prop_sameMatchFlip :: Eq a => [a] -> [a] -> Bool
prop_sameMatchFlip xs ys = sameMatch xs ys == sameMatch ys xs

prop_matchups2UniquePairs :: Eq a => [a] -> Bool
prop_matchups2UniquePairs = all unique . matchups2

prop_matchups2Length :: Eq a => [a] -> Bool
prop_matchups2Length = all ((== 2) . length) . matchups2

prop_bcdRoundtrip :: Int -> Int -> Bool
prop_bcdRoundtrip n = decodeBcd `inverts` encodeBcd n


return []
runTests :: IO Bool
runTests = $quickCheckAll