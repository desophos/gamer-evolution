{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
module TestUtil where

import           Data.ByteString.Builder        ( toLazyByteString )
import           Data.List                      ( sort )
import qualified Data.Set                      as S
import           Instances                      ( )
import           Test.Invariant                 ( inverts )
import           Test.QuickCheck                ( Arbitrary(arbitrary)
                                                , CoArbitrary
                                                , Gen
                                                , NonNegative
                                                    ( NonNegative
                                                    , getNonNegative
                                                    )
                                                , chooseInt
                                                , listOf1
                                                , quickCheckAll
                                                , resize
                                                , suchThat
                                                )
import           Util


newtype ChunkArgs a = ChunkArgs (Int, [a]) deriving (Eq, Show)

instance Arbitrary a => Arbitrary (ChunkArgs a) where
    arbitrary = do
        xs <- listOf1 arbitrary
        let len        = length xs
            lenFactors = [ i | i <- [1 .. len], len `mod` i == 0 ]
        n <- arbitrary `suchThat` (`elem` lenFactors)
        return $ ChunkArgs (n, xs)

newtype MatchupsArgs a = MatchupsArgs (Int, S.Set a) deriving (Eq, Show)

instance (Ord a, Arbitrary a) => Arbitrary (MatchupsArgs a) where
    arbitrary = do
        xs <- resize 50 arbitrary `suchThat` ((> 1) . S.size)
        k  <- chooseInt (2, 4) `suchThat` (<= S.size xs)
        return $ MatchupsArgs (k, xs)

newtype MatchBestArgs a = MatchBestArgs (Int, Int, a -> Int, S.Set a) deriving (Show)

instance (Ord a, Arbitrary a, CoArbitrary a) => Arbitrary (MatchBestArgs a) where
    arbitrary = do
        MatchupsArgs (k, xs) <- arbitrary :: Gen (MatchupsArgs a)
        n                    <- chooseInt (1, S.size xs - 1)
        f                    <- arbitrary
        return $ MatchBestArgs (k, n, f, xs)


prop_mergeAllNeighbors :: (Eq a, Arbitrary a, CoArbitrary a) => [a] -> Gen Bool
prop_mergeAllNeighbors xs = do
    let eqNeighbor []           = False
        eqNeighbor [_         ] = False
        eqNeighbor (x : y : ys) = x == y || eqNeighbor (y : ys)
    f <- arbitrary
    return . not . eqNeighbor $ mergeAll f xs

prop_chunkConcats :: Eq a => ChunkArgs a -> Bool
prop_chunkConcats (ChunkArgs (n, xs)) = concat `inverts` chunk n $ xs

prop_chunkUniform :: ChunkArgs a -> Bool
prop_chunkUniform (ChunkArgs (n, xs)) = all ((== n) . length) (chunk n xs)

prop_unique :: Ord a => [a] -> Bool
prop_unique xs = (unique xs && setEq xs) || not (unique xs || setEq xs)
    where setEq ys = sort ys == (S.toList . S.fromList) ys

prop_sameMatchFlip :: Ord a => [a] -> [a] -> Bool
prop_sameMatchFlip xs ys = sameMatch xs ys == sameMatch ys xs

prop_matchupsUnique :: Ord a => MatchupsArgs a -> Bool
prop_matchupsUnique (MatchupsArgs (k, xs)) = unique $ matchups k xs

prop_matchupsLength :: MatchupsArgs a -> Bool
prop_matchupsLength (MatchupsArgs (k, xs)) =
    all ((== k) . length) (matchups k xs)

prop_matchBestUnique :: Ord a => MatchBestArgs a -> Bool
prop_matchBestUnique (MatchBestArgs (k, n, f, xs)) =
    unique $ matchWithBest k n f xs

prop_matchBestLength :: MatchBestArgs a -> Bool
prop_matchBestLength (MatchBestArgs (k, n, f, xs)) =
    all ((== k) . length) (matchWithBest k n f xs)

prop_bcdRoundtrip :: NonNegative Int -> NonNegative Int -> Bool
prop_bcdRoundtrip (NonNegative n) =
    ((decodeBcd . toLazyByteString) `inverts` encodeBcd n) . getNonNegative


return []
runTests :: IO Bool
runTests = $quickCheckAll
