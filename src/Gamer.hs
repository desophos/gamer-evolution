{-# LANGUAGE RecordWildCards #-}

module Gamer
    ( newPlayers
    , playGame
    ) where

import           Control.Applicative
import           Control.Monad.State
import           Evolution
import           Test.QuickCheck
import           Util


type Action = Int
type StateID = Int
type Game = [Int] -> [Int]
data TransitionTree s = NextState s | Reactions [TransitionTree s]
    deriving (Eq, Show)
type StateTransitionTree = TransitionTree StateID

data PlayerState = PlayerState
    { playerAction      :: !Action
    , playerTransitions :: !StateTransitionTree
    }
    deriving (Eq, Show)

data GameState = GameState
    { gameStates    :: ![PlayerState]
    , gameHistories :: ![[Action]]
    , gameScores    :: ![Int]
    }
    deriving (Eq, Show)


maxAction :: Action
maxAction = 1

numActions :: Action
numActions = maxAction + 1

actionBcdLength :: Int
actionBcdLength = length $ encodeBcd 0 maxAction

maxState :: StateID
maxState = 7

stateIdBcdLength :: Int
stateIdBcdLength = length $ encodeBcd 0 maxState

memory :: Int
memory = 1

stateBcdLength :: Int
stateBcdLength = actionBcdLength + stateIdBcdLength * numActions * memory


encodeTransitions :: StateTransitionTree -> String
encodeTransitions (NextState i ) = encodeBcd stateIdBcdLength i
encodeTransitions (Reactions ts) = concatMap encodeTransitions ts

decodeTransitions :: String -> StateTransitionTree
decodeTransitions =
    let buildTree [x] = x
        buildTree xs  = buildTree . map Reactions . chunk numActions $ xs
    in  buildTree . map (NextState . decodeBcd) . chunk stateIdBcdLength


encodeState :: PlayerState -> String
encodeState PlayerState {..} = encodeBcd actionBcdLength playerAction
    ++ encodeTransitions playerTransitions

decodeState :: String -> PlayerState
decodeState s = PlayerState { .. }  where
    (action, transitions) = splitAt actionBcdLength s
    playerAction          = decodeBcd action
    playerTransitions     = decodeTransitions transitions


encodeChromosome :: [PlayerState] -> String
encodeChromosome = concatMap encodeState

decodeChromosome :: String -> [PlayerState]
decodeChromosome = map decodeState . chunk stateBcdLength


randomAction :: Gen Action
randomAction = choose (0, maxAction)

randomStateID :: Gen StateID
randomStateID = choose (0, maxState)

-- we need a number of recursive calls equal to the number of actions
-- i.e. number of branches = numActions
-- cons numActions recursive calls together
randomStateTransitionTree :: Int -> Gen StateTransitionTree
randomStateTransitionTree 0 =
    Reactions <$> vectorOf numActions (fmap NextState randomStateID)
randomStateTransitionTree depth =
    let
        actionBranches :: Int -> Gen [StateTransitionTree]
        actionBranches 0 = pure []
        actionBranches n = liftA2 (:)
                                  (randomStateTransitionTree (depth - 1))
                                  (actionBranches (n - 1))
    in
        Reactions <$> actionBranches numActions

-- depth = memory-1
randomState :: Gen PlayerState
randomState = do
    playerAction      <- randomAction
    playerTransitions <- randomStateTransitionTree (memory - 1)
    pure PlayerState { .. }

randomChromosome :: Gen [PlayerState]
randomChromosome = vectorOf (maxState + 1) randomState


newPlayers :: Int -> Gen [Agent [PlayerState]]
newPlayers n =
    newPopulation n encodeChromosome decodeChromosome <$> randomChromosome


findTransition :: StateTransitionTree -> [Action] -> StateID
findTransition (NextState stateID) _ = stateID
findTransition (Reactions transitions) (lastMove : restMoves) =
    findTransition (transitions !! lastMove) restMoves

nextState :: [PlayerState] -> PlayerState -> [Action] -> PlayerState
nextState states PlayerState {..} opponentHistory =
    states !! findTransition playerTransitions opponentHistory


stepPlayer :: [PlayerState] -> [Action] -> State PlayerState Action
stepPlayer chromosome opponentHistory = do
    oldState <- get
    let next = nextState chromosome oldState opponentHistory
    put next
    return $ playerAction next

stepGame :: Game -> Int -> [[PlayerState]] -> State GameState [Int]
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

playGame :: Game -> Int -> [[PlayerState]] -> [Int]
playGame game n players =
    let gameStates    = map head players
        actions       = map playerAction gameStates
        gameHistories = map (: []) actions
        gameScores    = game actions
    in  evalState (stepGame game (n - 1) players) GameState { .. }
