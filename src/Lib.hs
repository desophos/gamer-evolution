{-# LANGUAGE RecordWildCards #-}

module Lib
    ( encodeBcd, decodeBcd, randomChromosome, playGame
    ) where

import Numeric
import Data.Char
import Control.Applicative
import Control.Monad.State
import Test.QuickCheck
--import Debug.Trace
import Util

type Action = Int
type StateID = Int
data TransitionTree s = NextState s | Reactions [TransitionTree s]
    deriving (Eq, Show)
type StateTransitionTree = TransitionTree StateID
data PlayerState = PlayerState
    { playerAction :: !Action
    , playerTransitions :: !StateTransitionTree
    } deriving (Eq, Show)

type Game = [Int] -> [Int]
data GameState = GameState
    { gameStates :: ![PlayerState]
    , gameHistories :: ![[Action]]
    , gameScores :: ![Int]
    } deriving (Eq, Show)

maxAction :: Action
maxAction = 1

numActions :: Action
numActions = maxAction + 1

maxState :: StateID
maxState = 7

memory :: Int
memory = 1





randomAction :: Gen Action
randomAction = choose (0, maxAction)

randomStateID :: Gen StateID
randomStateID = choose (0, maxState)


-- we need a number of recursive calls equal to the number of actions
-- i.e. number of branches = numActions
-- cons numActions recursive calls together
randomStateTransitionTree :: Int -> Gen StateTransitionTree
randomStateTransitionTree 0 = Reactions <$> vectorOf numActions (fmap NextState randomStateID)
randomStateTransitionTree depth =
    let
        actionBranches :: Int -> Gen [StateTransitionTree]
        actionBranches 0 = pure []
        actionBranches n = liftA2 (:) (randomStateTransitionTree (depth-1)) (actionBranches (n-1))
    in Reactions <$> actionBranches numActions


-- depth = memory-1
randomState :: Gen PlayerState
randomState = do
    playerAction <- randomAction
    playerTransitions <- randomStateTransitionTree (memory-1)
    pure PlayerState{..}


randomChromosome :: Gen [PlayerState]
randomChromosome = vectorOf (maxState+1) randomState


findTransition :: StateTransitionTree -> [Action] -> StateID
findTransition (NextState stateID) _ = stateID
findTransition (Reactions transitions) (lastMove:restMoves) =
    findTransition (transitions !! lastMove) restMoves


nextState :: [PlayerState] -> PlayerState -> [Action] -> PlayerState
nextState states PlayerState{..} opponentHistory =
    states !! findTransition playerTransitions opponentHistory


stepPlayer :: [PlayerState] -> [Action] -> State PlayerState Action
stepPlayer chromosome opponentHistory = do
    oldState <- get
    let next = nextState chromosome oldState opponentHistory
    put next
    return $ playerAction next


playGame :: Game -> Int -> [[PlayerState]] -> [Int]
playGame game n players =
    let gameStates = map head players
        actions = map playerAction gameStates
        gameHistories = map (:[]) actions
        gameScores = game actions
    in evalState (stepGame game (n-1) players) GameState{..}


stepGame :: Game -> Int -> [[PlayerState]] -> State GameState [Int]
stepGame _ 0 _ = do
    GameState{..} <- get
    return gameScores

stepGame game n players = do
    GameState{..} <- get
    -- TODO: hacky, only works for 2 players
    -- reverse histories to pair each player with the other player's history
    let step (player, history, oldState) = runState (stepPlayer player history) oldState
        (actions, newStates) = unzip $ map step $ zip3 players (reverse gameHistories) gameStates
    put GameState { gameStates = newStates
                  , gameHistories = zipWith (:) actions gameHistories
                  , gameScores = zipWith (+) gameScores $ game actions
                  }
    stepGame game (n-1) players
