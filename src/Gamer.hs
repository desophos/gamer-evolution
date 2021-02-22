{-# LANGUAGE RecordWildCards #-}

module Gamer
    ( GamerParams(..)
    , newPlayers
    , playGame
    , dilemma
    ) where

import           Control.Applicative            ( Applicative(liftA2) )
import           Control.Monad.State            ( MonadState(get, put)
                                                , State
                                                , evalState
                                                , runState
                                                )
import           Evolution                      ( Agent
                                                , newPopulation
                                                )
import           Test.Invariant                 ( inverts )
import           Test.QuickCheck                ( Arbitrary(arbitrary)
                                                , Gen
                                                , choose
                                                , vectorOf
                                                )
import           Util                           ( chunk
                                                , decodeBcd
                                                , encodeBcd
                                                )


data GamerParams = GamerParams
    { gamerActions :: !Int -- ^ number of possible actions a player can take
    , gamerStates  :: !Int -- ^ number of states in a player's chromosome
    , gamerMemory  :: !Int -- ^ how many rounds back a player will remember its opponent's actions
    }
    deriving (Eq, Show)

instance Arbitrary GamerParams where
    arbitrary = do
        gamerActions <- choose (2, 8)
        gamerStates  <- (2 ^) <$> choose (1, 6 :: Int)
        gamerMemory  <- choose (1, 3)
        return GamerParams { .. }

data TransitionTree a = NextState a | Reactions [TransitionTree a]
    deriving (Ord, Eq, Show)
type StateTransitionTree = TransitionTree Int

data PlayerState = PlayerState
    { stateAction      :: !Int
    , stateTransitions :: !StateTransitionTree
    }
    deriving (Ord, Eq, Show)

data GameState = GameState
    { gameStates    :: ![PlayerState]
    , gameHistories :: ![[Int]]
    , gameScores    :: ![Int]
    }
    deriving (Ord, Eq, Show)


-- reward matrix for Prisoner's Dilemma
-- 0 = cooperate; 1 = defect
dilemma :: [Int] -> [Int]
dilemma [0, 0] = [3, 3]
dilemma [1, 0] = [5, 0]
dilemma [0, 1] = [0, 5]
dilemma [1, 1] = [1, 1]


-- | returns the number of binary digits required to represent
-- the largest value in [0..n-1] (which is n-1).
-- assumes it is passed the number of values in [0..n-1] (namely, n)
-- rather than the maximum value.
-- >>> bcdLen 16
-- 4
bcdLen :: Int -> Int
bcdLen = length . encodeBcd 0 . subtract 1


prop_encodedLen :: GamerParams -> Gen Bool
prop_encodedLen params@GamerParams {..} = do
    tree <- randomStateTransitionTree params
    let encoded = encodeTransitions params tree
    return $ 0 == length encoded `rem` bcdLen gamerStates

-- prop> \(params :: GamerParams) -> prop_encodedLen params
-- +++ OK, passed 100 tests.
encodeTransitions :: GamerParams -> StateTransitionTree -> String
encodeTransitions GamerParams {..} (NextState i) =
    encodeBcd (bcdLen gamerStates) i
encodeTransitions params (Reactions ts) =
    concatMap (encodeTransitions params) ts

-- prop> \(params :: GamerParams) -> (decodeTransitions params) `inverts` (encodeTransitions params) <$> randomStateTransitionTree params
-- +++ OK, passed 100 tests.
decodeTransitions :: GamerParams -> String -> StateTransitionTree
decodeTransitions GamerParams {..} =
    let buildTree xs = if length xs == gamerActions
            then Reactions xs
            else buildTree (map Reactions . chunk gamerActions $ xs)
    in  buildTree . map (NextState . decodeBcd) . chunk (bcdLen gamerStates)

encodeState :: GamerParams -> PlayerState -> String
encodeState params@GamerParams {..} PlayerState {..} =
    encodeBcd (bcdLen gamerActions) stateAction
        ++ encodeTransitions params stateTransitions

-- prop> \(params :: GamerParams) -> (decodeState params) `inverts` (encodeState params) <$> randomState params
-- +++ OK, passed 100 tests.
decodeState :: GamerParams -> String -> PlayerState
decodeState params@GamerParams {..} s = PlayerState { .. }  where
    (action, transitions) = splitAt (bcdLen gamerActions) s
    stateAction           = decodeBcd action
    stateTransitions      = decodeTransitions params transitions

prop_encodeUniformLen :: GamerParams -> Gen Bool
prop_encodeUniformLen params = do
    let encodedLen =
            length . encodeChromosome params <$> randomChromosome params
    cs <- vectorOf 20 encodedLen
    return $ all (head cs ==) cs

-- prop> \(params :: GamerParams) -> prop_encodeUniformLen params
-- +++ OK, passed 100 tests.
encodeChromosome :: GamerParams -> [PlayerState] -> String
encodeChromosome params = concatMap (encodeState params)

-- prop> \(params :: GamerParams) -> (decodeChromosome params) `inverts` (encodeChromosome params) <$> randomChromosome params
-- +++ OK, passed 100 tests.
decodeChromosome :: GamerParams -> String -> [PlayerState]
decodeChromosome params@GamerParams {..} = map (decodeState params)
    . chunk stateBcdLen
  where
    stateBcdLen =
        bcdLen gamerActions + bcdLen gamerStates * gamerActions ^ gamerMemory


prop_treeUniform :: GamerParams -> Gen Bool
prop_treeUniform params@GamerParams {..} = do
    let uniform (NextState _ ) = True
        uniform (Reactions xs) = length xs == gamerActions && all uniform xs
    tree <- randomStateTransitionTree params
    return $ uniform tree

-- prop> \(params :: GamerParams) -> prop_treeUniform params
-- +++ OK, passed 100 tests.
randomStateTransitionTree :: GamerParams -> Gen StateTransitionTree
randomStateTransitionTree GamerParams {..} = f (gamerMemory - 1)
  where
    -- we need a number of recursive calls equal to the number of actions
    -- i.e. number of branches = gamerActions
    f depth = Reactions <$> vectorOf gamerActions xs
      where
        xs = if depth <= 0
            then NextState <$> choose (0, gamerStates - 1)
            else f (depth - 1)

randomState :: GamerParams -> Gen PlayerState
randomState params@GamerParams {..} = do
    stateAction      <- choose (0, gamerActions - 1)
    stateTransitions <- randomStateTransitionTree params
    return PlayerState { .. }

randomChromosome :: GamerParams -> Gen [PlayerState]
randomChromosome params@GamerParams {..} =
    vectorOf gamerStates (randomState params)

newPlayers
    :: GamerParams
    -> Int -- ^ how many agents to generate
    -> Gen [Agent [PlayerState]]
newPlayers params n = newPopulation n
                                    (encodeChromosome params)
                                    (decodeChromosome params)
                                    (randomChromosome params)


-- | returns the index of the next state
findTransition
    :: StateTransitionTree -- ^ tree identifying potential states to transition to
    -> [Int] -- ^ the opponent's previous actions
    -> Int
findTransition (NextState stateID) _ = stateID
-- if memory is not full (fewer game rounds than memory)
-- then default to leftmost branch (action 0)
-- TODO: this isn't a great solution and may skew the algorithm
findTransition (Reactions transitions) [] =
    findTransition (head transitions) []
findTransition (Reactions transitions) (lastMove : restMoves) =
    findTransition (transitions !! lastMove) restMoves


nextState
    :: [PlayerState] -- ^ the FSM player
    -> PlayerState -- ^ the current state
    -> [Int] -- ^ the opponent's previous actions
    -> PlayerState
nextState states PlayerState {..} opponentHistory =
    states !! findTransition stateTransitions opponentHistory

-- | in the State monad, the state is the player's state
-- and the value is the player's action
stepPlayer
    :: [PlayerState] -- ^ the FSM player
    -> [Int] -- ^ the opponent's previous actions
    -> State PlayerState Int
stepPlayer chromosome opponentHistory = do
    oldState <- get
    let next = nextState chromosome oldState opponentHistory
    put next
    return $ stateAction next

stepGame
    :: ([Int] -> [Int]) -- ^ the game being played
    -> Int -- ^ number of rounds
    -> [[PlayerState]] -- ^ list of FSM players
    -> State GameState [Int]
stepGame _ 0 _ = do
    GameState {..} <- get
    return gameScores

stepGame game n players = do
    GameState {..} <- get
    -- TODO: hacky, only works for 2 players
    -- reverse histories to pair each player with the other player's history
    let step (player, history, oldState) =
            runState (stepPlayer player history) oldState
        (actions, newStates) =
            unzip $ map step $ zip3 players (reverse gameHistories) gameStates
    put GameState { gameStates    = newStates
                  , gameHistories = zipWith (:) actions gameHistories
                  , gameScores    = zipWith (+) gameScores $ game actions
                  }
    stepGame game (n - 1) players

playGame
    :: ([Int] -> [Int]) -- ^ the game being played
    -> Int -- ^ number of rounds
    -> [[PlayerState]] -- ^ list of FSM players
    -> [Int]
playGame game n players =
    let gameStates    = map head players
        actions       = map stateAction gameStates
        gameHistories = map (: []) actions
        gameScores    = game actions
    in  evalState (stepGame game (n - 1) players) GameState { .. }
