{-# LANGUAGE RecordWildCards, DeriveGeneric, ScopedTypeVariables #-}

module Evolution
    ( Agent(..), newPopulation, reproduce, mergeAgents
    ) where

import GHC.Generics (Generic)
import GHC.Float.RealFracMethods
import Data.List
import Control.Applicative
import Test.QuickCheck
import Test.Invariant
import Generic.Random
import Util


data Agent a = Agent
    { agentId :: !Int
    , agentFitness :: !Int
    , agentChromosome :: !a
    , agentEncoder :: !(a -> String)
    , agentDecoder :: !(String -> a)
    } deriving (Generic)

instance Eq (Agent a) where
    x == y = agentId x == agentId y
instance Ord (Agent a) where
    x <= y = agentId x <= agentId y
instance (Eq a, Arbitrary a, CoArbitrary a) => Arbitrary (Agent a) where
    arbitrary = do
        let agentId = 0
            agentFitness = 0
        agentChromosome <- arbitrary :: Gen a
        agentEncoder <- arbitrary :: Gen (a -> String)
        agentDecoder <- suchThat (arbitrary :: Gen (String -> a))
                                 (flip3 inverts agentChromosome agentEncoder)
        return Agent{..}
        where flip3 f x y z = f z y x


withId :: Agent a -> Int -> Agent a
withId x i = x { agentId = i }

withFitness :: Agent a -> Int -> Agent a
withFitness x i = x { agentFitness = i }


-- merges agents by ID, combining fitness
mergeAgents :: [Agent a] -> [Agent a]
mergeAgents = mergeAll f . sort
    where f x y = x {agentFitness = agentFitness x + agentFitness y}

-- runs a fitness function on a population of agents
getFitness :: ([a] -> [Int]) -> [Agent a] -> [Agent a]
getFitness f = mergeAgents . concatMap applyScores . matchups2
    where getScores = f . map agentChromosome
          applyScores xs = zipWith withFitness xs (getScores xs)


newAgent :: (a -> String) -> (String -> a) -> a -> Agent a
newAgent enc dec c = Agent
    { agentId = 0
    , agentFitness = 0
    , agentChromosome = c
    , agentEncoder = enc
    , agentDecoder = dec
    }

-- returns a population of agents with incremental IDs
newPopulation :: Int -> (a -> String) -> (String -> a) -> a -> [Agent a]
newPopulation n enc dec c =
    let agent = newAgent enc dec c
    in zipWith withId (replicate n agent) (iterate (+1) 0)


-- given 2 parent Agents, returns a child Agent whose chromosome is
crossover :: Agent a -> Agent a -> Gen (Agent a)
crossover x y = newAgent encoder decoder <$> newC where
    encoder = agentEncoder x
    decoder = agentDecoder x
    c = encoder . agentChromosome
    n = choose (1, length (c x) - 1)
    part1 = flip take (c x) <$> n
    part2 = flip drop (c y) <$> n
    newC = decoder <$> liftA2 (++) part1 part2

-- pSurvive: proportion of the population to survive to the next generation
-- f: fitness function to determine an Agent's likelihood to reproduce
-- pop: the population of Agents to reproduce
-- returns a population with non-surviving Agents replaced
-- by children produced by genetic crossover
reproduce :: Double -> ([a] -> [Int]) -> [Agent a] -> Gen [Agent a]
reproduce pSurvive f pop = (survived ++) <$> births where
    nSurvive = floorDoubleInt . (*) pSurvive . fromIntegral . length $ pop
    survived = sortOn agentId . take nSurvive . sortOn agentFitness . getFitness f $ pop
    fitPairs xs = zip (map agentFitness xs) (map pure xs)
    pickFit = frequency . fitPairs
    omit xs x = filter (\y -> agentId y /= agentId x) xs
    mate = do
        x <- pickFit survived
        y <- pickFit $ survived `omit` x
        crossover x y
    nBirth = length pop - nSurvive
    nextIds = iterate (+1) . agentId . last $ survived
    births = flip (zipWith withId) nextIds . replicate nBirth <$> mate
