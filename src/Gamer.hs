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
    deriving (Eq, Show)
type StateTransitionTree = TransitionTree Int

data PlayerState = PlayerState
    { stateAction      :: !Int
    , stateTransitions :: !StateTransitionTree
    }
    deriving (Eq, Show)

data GameState = GameState
    { gameStates    :: ![PlayerState]
    , gameHistories :: ![[Int]]
    , gameScores    :: ![Int]
    }
    deriving (Eq, Show)


findTransition :: StateTransitionTree -> [Action] -> StateID
findTransition (NextState stateID) _ = stateID
findTransition (Reactions transitions) (lastMove : restMoves) =
    findTransition (transitions !! lastMove) restMoves

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


encodeTransitions :: GamerParams -> StateTransitionTree -> String
encodeTransitions GamerParams {..} (NextState i) =
    encodeBcd (bcdLen gamerStates) i
encodeTransitions params (Reactions ts) =
    concatMap (encodeTransitions params) ts

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

decodeState :: GamerParams -> String -> PlayerState
decodeState params@GamerParams {..} s = PlayerState { .. }  where
    (action, transitions) = splitAt (bcdLen gamerActions) s
    stateAction           = decodeBcd action
    stateTransitions      = decodeTransitions params transitions

encodeChromosome :: GamerParams -> [PlayerState] -> String
encodeChromosome params = concatMap (encodeState params)

decodeChromosome :: GamerParams -> String -> [PlayerState]
decodeChromosome params@GamerParams {..} = map (decodeState params)
    . chunk stateBcdLen
  where
    stateBcdLen =
        bcdLen gamerActions + bcdLen gamerStates * gamerActions ^ gamerMemory


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
