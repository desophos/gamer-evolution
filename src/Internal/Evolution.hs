{-# LANGUAGE RecordWildCards, DeriveGeneric, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Internal.Evolution where

import           Control.Applicative            ( liftA2 )
import qualified Data.ByteString.Lazy          as B
import           Data.List                      ( sort
                                                , sortOn
                                                )
import           Data.MonoTraversable           ( MonoTraversable(omapM) )
import           Data.Ord                       ( Down(Down) )
import           Data.Word                      ( Word8 )
import           GHC.Float.RealFracMethods      ( roundDoubleInt )
import           GHC.Generics                   ( Generic )
import           Test.QuickCheck                ( Arbitrary(arbitrary)
                                                , Gen
                                                , choose
                                                , chooseInt
                                                , elements
                                                , frequency
                                                , suchThat
                                                , vectorOf
                                                )
import           Test.QuickCheck.Gen            ( chooseInt64 )
import           Util


data EvolutionParams = EvolutionParams
    { evolveGenerations :: !Int -- ^ Number of generations to evolve the population for.
    , evolvePopSize     :: !Int -- ^ Number of Agents in the population.
    , evolveSurvivors   :: !Int -- ^ Number of Agents that survive each generation (>= 2).
    , evolveMutateP     :: !Double -- ^ Probability of mutation per gene (0 <= p <= 1).
    }
    deriving (Eq, Show)

instance Arbitrary EvolutionParams where
    arbitrary = do
        evolveGenerations <- chooseInt (30, 50)
        evolvePopSize     <- chooseInt (40, 60) `suchThat` even
        let both f (x, y) = (f x, f y)
            scalePopSize = roundDoubleInt . (* fromIntegral evolvePopSize)
            surviveRange = both scalePopSize (0.4, 0.6)
        evolveSurvivors <- chooseInt surviveRange
            `suchThat` combineWith (&&) [(> 1), (< evolvePopSize)]
        evolveMutateP <- choose (0, 0.05) `suchThat` (> 0) -- avoid division by zero in analysis
        return EvolutionParams { .. }


data Agent a = Agent
    { agentId      :: !Int
    , agentFitness :: !Float
    , agentPhenome :: !a -- ^ The representation of an Agent's expressed traits, e.g. behavior.
    , agentGenes   :: ![Word8] -- ^ The possible characters that make up the encoded genome. Length should be >= 2.
    , agentEncoder :: !(a -> B.ByteString)
    , agentDecoder :: !(B.ByteString -> a)
    }
    deriving Generic

instance Eq (Agent a) where
    x == y = agentId x == agentId y
instance Ord (Agent a) where
    x <= y = agentId x <= agentId y
instance Arbitrary (Agent [Word8]) where
    -- ByteString has no Arbitrary instance so use [Word8] instead.
    -- Restricting the phenome to [Word8] provides enough complexity for evolution
    -- and lets us use a trivial encoder/decoder instead of generating functions.
    arbitrary = do
        let agentId      = 0
            agentFitness = 0
            agentEncoder = B.pack
            agentDecoder = B.unpack
        agentGenes <- arbitrary
            `suchThat` combineWith (&&) [unique, (> 1) . length]
        numGenes     <- chooseInt (2, 30)
        agentPhenome <- vectorOf numGenes (elements agentGenes)
        return Agent { .. }


genAgentLike :: Agent [Word8] -> Gen (Agent [Word8])
genAgentLike agent@Agent {..} = do
    agentPhenome' <- vectorOf (length agentPhenome) (elements agentGenes)
    return agent { agentId = agentId + 1, agentPhenome = agentPhenome' }


-- trivial helper functions
withId :: Agent a -> Int -> Agent a
withId x i = x { agentId = i }

withFitness :: Agent a -> Float -> Agent a
withFitness x i = x { agentFitness = i }

genomeOf :: Agent a -> B.ByteString
genomeOf Agent {..} = agentEncoder agentPhenome


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
    getScores = f . map agentPhenome
    applyScores xs = zipWith withFitness xs (getScores xs)


-- | Returns the average fitness of a population.
avgFitness :: [Agent a] -> Float
avgFitness = combineWith (/) [sum . map agentFitness, realToFrac . length]


newAgent
    :: (a -> B.ByteString) -> (B.ByteString -> a) -> [Word8] -> a -> Agent a
newAgent agentEncoder agentDecoder agentGenes agentPhenome = Agent { .. }
  where
    agentId      = 0
    agentFitness = 0


-- | Returns a population of agents with incremental IDs.
genPopulation
    :: Int -- ^ Number of agents to generate.
    -> [Word8] -- ^ Possible genes that make up the encoded genome.
    -> (a -> B.ByteString) -- ^ Encodes an agent's phenome into its genome.
    -> (B.ByteString -> a) -- ^ Decodes an agent's genome into its phenome. Should invert the encoder.
    -> Gen a -- ^ Phenome generator.
    -> Gen [Agent a]
genPopulation n genes enc dec genPhenome = do
    let agent = newAgent enc dec genes <$> genPhenome
    agents <- vectorOf n agent
    return $ zipWith withId agents [0 ..]


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
        part1     = flip B.take (genomeOf x)
        part2     = flip B.drop (genomeOf y)
        combineAt = combineWith (<>) [part1, part2]
    crosspoint <- chooseInt64 (1, B.length (genomeOf x) - 1)
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
            . sortOn (Down . agentFitness)
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
    evolveGenerations
    (pure pop)
  where
    reproduce' = reproduce params f matchup
    evolve' n xs = if n <= 0 then xs else evolve' (n - 1) . reproduce' =<< xs


-- | Like 'evolve' but returns a list of all generations instead of just the last one.
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
