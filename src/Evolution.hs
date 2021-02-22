{-# LANGUAGE RecordWildCards, DeriveGeneric, ScopedTypeVariables #-}

module Evolution
    ( Agent(..)
    , newPopulation
    , reproduce
    , evolve
    , mergeAgents
    ) where

import           Data.List                      ( sort
                                                , sortOn
                                                )
import           GHC.Float.RealFracMethods      ( floorDoubleInt )
import           GHC.Generics                   ( Generic )
import           Test.Invariant                 ( inverts )
import           Test.QuickCheck                ( Arbitrary(arbitrary)
                                                , CoArbitrary
                                                , Gen
                                                , choose
                                                , frequency
                                                , suchThat
                                                , vectorOf
                                                )
import           Util                           ( combineWith
                                                , mergeAll
                                                )


data Agent a = Agent
    { agentId         :: !Int
    , agentFitness    :: !Int
    , agentChromosome :: !a
    , agentEncoder    :: !(a -> String)
    , agentDecoder    :: !(String -> a)
    }
    deriving Generic

instance Eq (Agent a) where
    x == y = agentId x == agentId y
instance Ord (Agent a) where
    x <= y = agentId x <= agentId y
instance (Eq a, Arbitrary a, CoArbitrary a) => Arbitrary (Agent a) where
    arbitrary = do
        let flip3 f x y z = f z y x
            agentId      = 0
            agentFitness = 0
        agentChromosome <- arbitrary
        agentEncoder    <- arbitrary
        agentDecoder    <-
            arbitrary `suchThat` flip3 inverts agentChromosome agentEncoder
        return Agent { .. }


-- trivial helper functions
withId :: Agent a -> Int -> Agent a
withId x i = x { agentId = i }

withFitness :: Agent a -> Int -> Agent a
withFitness x i = x { agentFitness = i }


-- merges agents by ID, combining fitness
mergeAgents :: [Agent a] -> [Agent a]
mergeAgents = mergeAll f . sort
    where f x y = x { agentFitness = agentFitness x + agentFitness y }

-- runs a fitness function on a population of agents
-- takes a list of Agent "matchups" to pass to the fitness function
-- returns the population with updated agentFitness
getFitness :: ([a] -> [Int]) -> [[Agent a]] -> [Agent a]
getFitness f = mergeAgents . concatMap applyScores
  where
    getScores = f . map agentChromosome
    applyScores xs = zipWith withFitness xs (getScores xs)


newAgent :: (a -> String) -> (String -> a) -> a -> Agent a
newAgent enc dec c = Agent { agentId         = 0
                           , agentFitness    = 0
                           , agentChromosome = c
                           , agentEncoder    = enc
                           , agentDecoder    = dec
                           }

-- returns a population of agents with incremental IDs
newPopulation
    :: Int -> (a -> String) -> (String -> a) -> Gen a -> Gen [Agent a]
newPopulation n enc dec c = do
    let agent = newAgent enc dec <$> c
    agents <- vectorOf n agent
    return $ zipWith withId agents (iterate (+ 1) 0)


-- given 2 parent Agents, returns a child Agent whose chromosome is
-- produced via a simulation of genetic crossover
crossover :: Agent a -> Agent a -> Gen (Agent a)
crossover x y = do
    let encoder = agentEncoder x
        decoder = agentDecoder x
        c       = encoder . agentChromosome
        part1   = flip take (c x)
        part2   = flip drop (c y)
        newC    = decoder . combineWith (++) [part1, part2]
    crosspoint <- choose (1, length (c x) - 1)
    return $ newAgent encoder decoder (newC crosspoint)

-- pSurvive: proportion of the population to survive to the next generation
--           minimum 2 members
-- f: fitness function to determine an Agent's likelihood to reproduce
-- matchup: function to generate a list of "matchups" for `f`.
-- pop: the population of Agents to reproduce
-- returns a population with non-surviving Agents replaced
-- by children produced by genetic crossover
reproduce
    :: Double
    -> ([a] -> [Int])
    -> ([Agent a] -> [[Agent a]])
    -> [Agent a]
    -> Gen [Agent a]
reproduce pSurvive f matchup pop = (survived ++) <$> births
  where
    nSurvive =
        max 2 . floorDoubleInt . (*) pSurvive . fromIntegral . length $ pop
    survived =
        sortOn agentId
            . take nSurvive
            . sortOn agentFitness
            . getFitness f
            . matchup
            $ pop
    fitPairs xs = zip (map agentFitness xs) (map pure xs)
    pickFit = frequency . fitPairs
    omit xs x = filter (\y -> agentId y /= agentId x) xs
    mate = do
        x <- pickFit survived
        y <- pickFit $ survived `omit` x
        crossover x y
    nBirth  = length pop - nSurvive
    nextIds = iterate (+ 1) . (+ 1) . agentId . last $ survived
    births  = flip (zipWith withId) nextIds . replicate nBirth <$> mate

evolve
    :: Double
    -> ([a] -> [Int])
    -> ([Agent a] -> [[Agent a]])
    -> [Agent a]
    -> Int
    -> Gen [Agent a]
evolve pSurvive f matchup pop = evolve' (pure pop)
  where
    reproduce' = reproduce pSurvive f matchup
    evolve' xs n = if n <= 0 then xs else evolve' (reproduce' =<< xs) (n - 1)
