{-# LANGUAGE RecordWildCards #-}

module Internal.Gamer where

import           Control.Monad.State            ( MonadState(get, put)
                                                , State
                                                , evalState
                                                , runState
                                                )
import           Data.ByteString.Builder        ( Builder
                                                , intDec
                                                , toLazyByteString
                                                )
import qualified Data.ByteString.Lazy          as B
import           Evolution                      ( Agent
                                                , genPopulation
                                                )
import           Test.QuickCheck                ( Arbitrary(arbitrary)
                                                , Gen
                                                , choose
                                                , vectorOf
                                                )
import           Util                           ( chunk
                                                , decodeBcd
                                                , encodeBcd
                                                , memoize
                                                )


data GamerParams = GamerParams
    { gamerActions :: !Int -- ^ number of possible actions a player can take
    , gamerStates  :: !Int -- ^ number of states in a player's genome
    , gamerMemory  :: !Int -- ^ how many rounds back a player will remember its opponent's actions
    }
    deriving (Eq, Show)

instance Arbitrary GamerParams where
    arbitrary = do
        gamerActions <- choose (2, 2)
        gamerStates  <- (2 ^) <$> choose (1, 3 :: Int)
        gamerMemory  <- choose (3, 3)
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
    , gameScores    :: ![Float]
    }
    deriving (Ord, Eq, Show)

{- $setup
>>> :set -XRecordWildCards
>>> :{
let prop_encodedLen params@GamerParams {..} = do
        tree <- genStateTransitionTree params
        let encoded = encodeTransitions params tree
            bsLen   = fromIntegral . B.length . toLazyByteString
        return $ 0 == bsLen encoded `rem` bcdLen gamerStates
    prop_encodeUniformLen params = do
        let encodedLen = B.length . encodeGenome params <$> genGenome params
        cs <- vectorOf 20 encodedLen
        return $ all (head cs ==) cs
    prop_treeUniform params@GamerParams {..} = do
        let uniform (NextState _ ) = True
            uniform (Reactions xs) = length xs == gamerActions && all uniform xs
        tree <- genStateTransitionTree params
        return $ uniform tree
:}
-}

-- | Reward matrix for the Prisoner's Dilemma.
-- 0 = cooperate; 1 = defect
dilemma :: [Int] -> [Float]
dilemma [0, 0] = [3, 3]
dilemma [1, 0] = [5, 0]
dilemma [0, 1] = [0, 5]
dilemma [1, 1] = [1, 1]


-- | Returns the number of binary digits required to represent
-- the largest value in [0..n-1] (which is n-1).
-- Assumes it is passed the number of values in [0..n-1] (namely, n)
-- rather than the maximum value.
-- >>> bcdLen 16
-- 4
bcdLen :: Int -> Int
bcdLen =
    memoize
        $ fromIntegral
        . B.length
        . toLazyByteString
        . encodeBcd 0
        . subtract 1


-- prop> \(params :: GamerParams) -> prop_encodedLen params
-- +++ OK, passed 100 tests.
encodeTransitions :: GamerParams -> StateTransitionTree -> Builder
encodeTransitions GamerParams {..} (NextState i) =
    encodeBcd (bcdLen gamerStates) i
encodeTransitions params (Reactions ts) =
    mconcat $ map (encodeTransitions params) ts

-- prop> \(params :: GamerParams) -> (decodeTransitions params) `inverts` (encodeTransitions params) <$> genStateTransitionTree params
-- +++ OK, passed 100 tests.
decodeTransitions :: GamerParams -> B.ByteString -> StateTransitionTree
decodeTransitions GamerParams {..} =
    let buildTree xs = if length xs == gamerActions
            then Reactions xs
            else buildTree (map Reactions . chunk gamerActions $ xs)
    in  buildTree . map (NextState . decodeBcd) . chunk (bcdLen gamerStates)

encodeState :: GamerParams -> PlayerState -> Builder
encodeState params@GamerParams {..} PlayerState {..} =
    encodeBcd (bcdLen gamerActions) stateAction
        <> encodeTransitions params stateTransitions

-- prop> \(params :: GamerParams) -> (decodeState params) `inverts` (encodeState params) <$> genState params
-- +++ OK, passed 100 tests.
decodeState :: GamerParams -> B.ByteString -> PlayerState
decodeState params@GamerParams {..} s = PlayerState { .. }  where
    (action, transitions) = B.splitAt (fromIntegral $ bcdLen gamerActions) s
    stateAction           = decodeBcd action
    stateTransitions      = decodeTransitions params transitions

-- prop> \(params :: GamerParams) -> prop_encodeUniformLen params
-- +++ OK, passed 100 tests.
encodeGenome :: GamerParams -> [PlayerState] -> B.ByteString
encodeGenome params = toLazyByteString . mconcat . map (encodeState params)

-- prop> \(params :: GamerParams) -> (decodeGenome params) `inverts` (encodeGenome params) <$> genGenome params
-- +++ OK, passed 100 tests.
decodeGenome :: GamerParams -> B.ByteString -> [PlayerState]
decodeGenome params@GamerParams {..} = map (decodeState params)
    . chunk stateBcdLen
  where
    stateBcdLen =
        bcdLen gamerActions + bcdLen gamerStates * gamerActions ^ gamerMemory


-- prop> \(params :: GamerParams) -> prop_treeUniform params
-- +++ OK, passed 100 tests.
genStateTransitionTree :: GamerParams -> Gen StateTransitionTree
genStateTransitionTree GamerParams {..} = f (gamerMemory - 1)
  where
    -- we need a number of recursive calls equal to the number of actions
    -- i.e. number of branches = gamerActions
    f depth = Reactions <$> vectorOf gamerActions xs
      where
        xs = if depth <= 0
            then NextState <$> choose (0, gamerStates - 1)
            else f (depth - 1)

genState :: GamerParams -> Gen PlayerState
genState params@GamerParams {..} = do
    stateAction      <- choose (0, gamerActions - 1)
    stateTransitions <- genStateTransitionTree params
    return PlayerState { .. }

genGenome :: GamerParams -> Gen [PlayerState]
genGenome params@GamerParams {..} = vectorOf gamerStates (genState params)

genPlayers
    :: GamerParams
    -> Int -- ^ Number of agents to generate.
    -> Gen [Agent [PlayerState]]
genPlayers params n = genPopulation
    n
    (B.unpack . toLazyByteString . mconcat $ map intDec [0, 1])
    (encodeGenome params)
    (decodeGenome params)
    (genGenome params)


-- | Returns the index of the next PlayerState.
findTransition
    :: StateTransitionTree -- ^ Tree identifying potential states to transition to.
    -> [Int] -- ^ The opponent's previous actions (sorted from most to least recent).
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
    :: [PlayerState] -- ^ The FSM player.
    -> PlayerState -- ^ The current state.
    -> [Int] -- ^ The opponent's previous actions.
    -> PlayerState
nextState states PlayerState {..} opponentHistory =
    states !! findTransition stateTransitions opponentHistory

-- | In the State monad, the state is the player's state
-- and the value is the player's action.
stepPlayer
    :: [PlayerState] -- ^ The FSM player.
    -> [Int] -- ^ The opponent's previous actions.
    -> State PlayerState Int
stepPlayer genome opponentHistory = do
    oldState <- get
    let next = nextState genome oldState opponentHistory
    put next
    return $ stateAction next

stepGame
    :: ([Int] -> [Float]) -- ^ The game being played.
    -> Int -- ^ Number of rounds to play.
    -> [[PlayerState]] -- ^ List of FSM players.
    -> State GameState [Float]
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
                  , gameScores    = zipWith (+) gameScores (game actions)
                  }
    stepGame game (n - 1) players

playGame
    :: ([Int] -> [Float]) -- ^ The game being played. Must preserve list length.
    -> Int -- ^ Number of rounds to play.
    -> [[PlayerState]] -- ^ List of FSM players.
    -> [Float]
playGame game n players =
    let gameStates    = map head players
        actions       = map stateAction gameStates
        gameHistories = map (: []) actions
        gameScores    = game actions
    in  evalState (stepGame game (n - 1) players) GameState { .. }
