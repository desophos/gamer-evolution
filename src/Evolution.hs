{-# LANGUAGE RecordWildCards, DeriveGeneric, ScopedTypeVariables #-}

module Evolution
    ( Agent(..)
    , EvolutionParams(..)
    , genPopulation
    , mutate
    , reproduce
    , evolve
    , collectEvolve
    , mergeAgents
    , getFitness
    ) where

import           Control.Applicative            ( liftA2 )
import qualified Data.ByteString.Lazy          as B
import           Data.List                      ( sort
                                                , sortOn
                                                )
import           Data.MonoTraversable           ( MonoTraversable(omapM) )
import           Data.Word                      ( Word8 )
import           GHC.Float.RealFracMethods      ( roundDoubleInt )
import           GHC.Generics                   ( Generic )
import           Test.Invariant                 ( inverts )
import           Test.QuickCheck                ( Arbitrary(arbitrary)
                                                , CoArbitrary
                                                , Gen
                                                , choose
                                                , chooseInt
                                                , elements
                                                , frequency
                                                , suchThat
                                                , vectorOf
                                                )
import           Test.QuickCheck.Gen            ( chooseInt64 )
import           Util                           ( combineWith
                                                , mergeAll
                                                , omit
                                                , unique
                                                )


data EvolutionParams = EvolutionParams
    { evolveGenerations :: !Int -- ^ Number of generations to evolve the population for.
    , evolvePopSize     :: !Int -- ^ Number of Agents in the population.
    , evolveSurvivors   :: !Int -- ^ Number of Agents that survive each generation (>= 2).
    , evolveMutateP     :: !Double -- ^ Probability of mutation per gene (0 <= p <= 1).
    }
    deriving (Eq, Show)

instance Arbitrary EvolutionParams where
    arbitrary = do
        evolveGenerations <- chooseInt (5, 10)
        evolvePopSize     <- chooseInt (20, 60) `suchThat` even
        let both f (x, y) = (f x, f y)
            scalePopSize = roundDoubleInt . (* fromIntegral evolvePopSize)
            surviveRange = both scalePopSize (0.3, 0.8)
        evolveSurvivors <- arbitrary `suchThat` combineWith
            (&&)
            [ (> 1)
            , (< evolvePopSize)
            , (> fst surviveRange)
            , (< snd surviveRange)
            ]
        evolveMutateP <- choose (0, 0.1) `suchThat` (> 0) -- avoid division by zero in analysis
        return EvolutionParams { .. }

data Agent a = Agent
    { agentId      :: !Int
    , agentFitness :: !Float
    , agentGenome  :: !a
    , agentGenes   :: ![Word8] -- ^ The possible characters that make up the encoded genome. Length should be >= 2.
    , agentEncoder :: !(a -> B.ByteString)
    , agentDecoder :: !(B.ByteString -> a)
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
        agentGenome <- arbitrary
        agentGenes  <- arbitrary
            `suchThat` combineWith (&&) [unique, (> 1) . length]
        -- ByteString has no Arbitrary instance 
        -- so generate functions over [Word8] instead
        agentEncoder <-
            (B.pack .)
            <$>        arbitrary
            `suchThat` (\f -> B.all (`elem` agentGenes)
                                    (B.pack . f $ agentGenome)
                       )
        agentDecoder <-
            (. B.unpack)
            <$>        arbitrary
            `suchThat` (\f -> inverts (f . B.unpack) agentEncoder agentGenome)
        return Agent { .. }


-- trivial helper functions
withId :: Agent a -> Int -> Agent a
withId x i = x { agentId = i }

withFitness :: Agent a -> Float -> Agent a
withFitness x i = x { agentFitness = i }


-- | Merges agents by ID, averaging fitness.
mergeAgents :: [Agent a] -> [Agent a]
mergeAgents = mergeAll f . sort
    where f x y = x { agentFitness = (agentFitness x + agentFitness y) / 2 }

-- | Runs a fitness function on a population of agents.
getFitness
    :: ([a] -> [Float]) -- ^ Fitness function.
    -> [[Agent a]] -- ^ Agent "matchups" to pass to the fitness function.
    -> [Agent a] -- ^ The population with updated agentFitness.
getFitness f = mergeAgents . concatMap applyScores
  where
    getScores = f . map agentGenome
    applyScores xs = zipWith withFitness xs (getScores xs)


newAgent
    :: (a -> B.ByteString) -> (B.ByteString -> a) -> [Word8] -> a -> Agent a
newAgent agentEncoder agentDecoder agentGenes agentGenome = Agent { .. }
  where
    agentId      = 0
    agentFitness = 0

-- | Returns a population of agents with incremental IDs.
genPopulation
    :: Int -- ^ Number of agents to generate.
    -> [Word8] -- ^ Possible genes that make up the encoded genome.
    -> (a -> B.ByteString) -- ^ Encodes an agent's genome.
    -> (B.ByteString -> a) -- ^ Decodes an agent's genome. Should invert the encoder.
    -> Gen a -- ^ Genome generator.
    -> Gen [Agent a]
genPopulation n genes enc dec genome = do
    let agent = newAgent enc dec genes <$> genome
    agents <- vectorOf n agent
    return $ zipWith withId agents (iterate (+ 1) 0)


mutate
    :: Double -- ^ Probability per gene of mutation.
    -> [Word8] -- ^ Possible genes to mutate into.
    -> B.ByteString -- ^ The genome to mutate.
    -> Gen B.ByteString
mutate pMutate genes = omapM f
  where
    f x = do
        p <- choose (0, 1)
        if p < pMutate then elements (genes `omit` x) else pure x

-- | Given 2 parent Agents, returns a child Agent whose chromosome is
-- produced via a simulation of genetic crossover and mutation.
crossover :: EvolutionParams -> Agent a -> Agent a -> Gen (Agent a)
crossover EvolutionParams {..} x y = do
    let encoder   = agentEncoder x
        decoder   = agentDecoder x
        genes     = agentGenes x
        encoded   = encoder . agentGenome
        part1     = flip B.take (encoded x)
        part2     = flip B.drop (encoded y)
        combineAt = combineWith (<>) [part1, part2]
    crosspoint <- chooseInt64 (1, B.length (encoded x) - 1)
    mutated    <- mutate evolveMutateP genes (combineAt crosspoint)
    return $ newAgent encoder decoder genes (decoder mutated)

reproduce
    :: EvolutionParams
    -> ([a] -> [Float]) -- ^ Fitness function to determine Agents' likelihood to reproduce. Must preserve list length.
    -> ([Agent a] -> [[Agent a]]) -- ^ Given the population, produces the list of groups passed to the fitness fn.
    -> [Agent a] -- ^ The population of Agents to reproduce.
    -> Gen [Agent a] -- ^ The population with non-surviving Agents replaced by children produced by sexual reproduction.
reproduce params@EvolutionParams {..} f matchup pop = do
    births <- vectorOf nBirth mate
    let births' = zipWith withId births (nextIds survived)
    return $ survived ++ births'
  where
    nBirth  = length pop - evolveSurvivors
    nextIds = iterate (+ 1) . (+ 1) . agentId . last
    survived =
        sortOn agentId
            . take evolveSurvivors
            . sortOn agentFitness
            . getFitness f
            . matchup
            $ pop
    fitPairs xs = zip (map (ceiling . agentFitness) xs) (map pure xs)
    pickFit = frequency . fitPairs
    mate    = do
        x <- pickFit survived
        y <- pickFit $ survived `omit` x
        crossover params x y

-- | Evolves a population over a number of generations. See 'reproduce'.
evolve
    :: EvolutionParams
    -> ([a] -> [Float])
    -> ([Agent a] -> [[Agent a]])
    -> [Agent a]
    -> Gen [Agent a]
evolve params@EvolutionParams {..} f matchup pop = evolve'
    (pure pop)
    evolveGenerations
  where
    reproduce' = reproduce params f matchup
    evolve' xs n = if n <= 0 then xs else evolve' (reproduce' =<< xs) (n - 1)

collectEvolve
    :: EvolutionParams
    -> ([a] -> [Float])
    -> ([Agent a] -> [[Agent a]])
    -> [Agent a]
    -> Gen [[Agent a]]
collectEvolve params@EvolutionParams {..} f matchup pop = evolve'
    (pure [])
    (pure pop)
    evolveGenerations
  where
    reproduce' = reproduce params f matchup
    evolve' acc xs n = if n <= 0
        then reverse <$> liftA2 (:) xs acc
        else evolve' (liftA2 (:) xs acc) (reproduce' =<< xs) (n - 1)
