{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant id" #-}

-- | Porting of https://github.com/thinkbeforecoding/UnoCore/blob/solution/Uno/Game.fs
module Crem.Example.Uno where

import Crem.BaseMachine (InitialState (..))
import Crem.Decider (Decider (..), EvolutionResult (..))
import Crem.Topology
import "singletons-base" Data.Singletons.Base.TH
import Prelude hiding (id, init, reverse)
import Prelude qualified (id)

-- * Domain

newtype PlayerCount = PlayerCount Int
  deriving newtype (Eq, Ord)

data Digit
  = Zero
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eigth
  | Nine
  deriving stock (Eq)

data Colour
  = Red
  | Green
  | Blue
  | Yellow
  deriving stock (Eq)

data Card
  = DigitCard Digit Colour
  | Skip Colour
  | Kickback Colour
  deriving stock (Eq)

colour :: Card -> Colour
colour (DigitCard _ c) = c
colour (Skip c) = c
colour (Kickback c) = c

sameColour :: Card -> Card -> Bool
sameColour card1 card2 =
  colour card1 == colour card2

data CardValue
  = DigitValue Digit
  | SkipValue
  | KickbackValue
  deriving stock (Eq)

value :: Card -> CardValue
value (DigitCard d _) = DigitValue d
value (Skip _) = SkipValue
value (Kickback _) = KickbackValue

sameValue :: Card -> Card -> Bool
sameValue card1 card2 =
  value card1 == value card2

newtype PlayerId = PlayerId Int
  deriving stock (Eq)

data Direction
  = Clockwise
  | CounterClockwise

reverseDirection :: Direction -> Direction
reverseDirection Clockwise = CounterClockwise
reverseDirection CounterClockwise = Clockwise

data Player = Player
  { id :: PlayerId
  , count :: PlayerCount
  , direction :: Direction
  }

init :: PlayerId -> PlayerCount -> Player
init playerId count =
  Player
    { id = playerId
    , count = count
    , direction = Clockwise
    }

next :: Player -> Player
next (Player (PlayerId id) (PlayerCount count) direction) =
  case direction of
    Clockwise ->
      Player
        { id = PlayerId $ (id + 1) `mod` count
        , count = PlayerCount count
        , direction
        }
    CounterClockwise ->
      Player
        { id = PlayerId $ (id - 1) `mod` count
        , count = PlayerCount count
        , direction
        }

skip :: Player -> Player
skip = next . next

reverse :: Player -> Player
reverse (Player id count direction) =
  Player id count $ reverseDirection direction

nextPlayer :: Card -> Player -> Player
nextPlayer card player = case card of
  DigitCard _ _ -> next player
  Skip _ -> skip player
  Kickback _ -> next . reverse $ player

set :: PlayerId -> Player -> Player
set playerId player = player {id = playerId}

-- * Commands and events

data InitialData = InitialData
  { players :: PlayerCount
  , firstCard :: Card
  }

data PlayData = PlayData
  { playerId :: PlayerId
  , card :: Card
  }

data Command
  = StartGame InitialData
  | PlayCard PlayData

data Event
  = GameStarted InitialData PlayerId
  | CardPlayed PlayData
  | CardPlayedAndTurnBegan PlayData PlayerId
  | WrongCardPlayed PlayData
  | PlayerPlayedAtWrongTurn PlayData

-- * Topology

$( singletons
    [d|
      data UnoVertex
        = Initial
        | Started

      unoTopology :: Topology UnoVertex
      unoTopology = Topology [(Initial, [Started])]
      |]
 )

-- * State

data StateData = StateData
  { topCard :: Card
  , currentPlayer :: Player
  }

data UnoState (vertex :: UnoVertex) where
  UnoInitialState :: UnoState 'Initial
  UnoStartedState :: StateData -> UnoState 'Started

-- * Errors

data GameError
  = TooFewPlayers
  | GameAlreadyStarted
  | GameNotStarted

-- * Machine

unoDecider :: Decider UnoTopology Command (Either GameError Event)
unoDecider =
  Decider
    { deciderInitialState = InitialState UnoInitialState
    , decide = \command state ->
        case (state, command) of
          (_, StartGame initialData)
            | players initialData < PlayerCount 2 ->
                Left TooFewPlayers
          (UnoStartedState _, StartGame _) ->
            Left GameAlreadyStarted
          (_, StartGame initialData) ->
            Right $
              GameStarted
                initialData
                ( id $
                    nextPlayer
                      (firstCard initialData)
                      (init (PlayerId 0) $ players initialData)
                )
          (UnoInitialState, PlayCard _) ->
            Left GameNotStarted
          (UnoStartedState stateData, PlayCard playData)
            | id (currentPlayer stateData) /= playerId playData ->
                if sameColour (topCard stateData) (card playData) && sameValue (topCard stateData) (card playData)
                  then Right $ CardPlayed playData
                  else Right $ PlayerPlayedAtWrongTurn playData
          (UnoStartedState stateData, PlayCard playData) ->
            if sameColour (topCard stateData) (card playData) || sameValue (topCard stateData) (card playData)
              then
                Right $
                  CardPlayedAndTurnBegan
                    playData
                    ( id $
                        nextPlayer
                          (card playData)
                          (currentPlayer stateData)
                    )
              else Right $ WrongCardPlayed playData
    , evolve = \state eitherErrorEvent ->
        case eitherErrorEvent of
          Left _ -> EvolutionResult state
          Right event -> case (state, event) of
            (UnoInitialState, GameStarted initialData playerId) ->
              initialResult initialData playerId
            (UnoStartedState _, GameStarted initialData playerId) ->
              initialResult initialData playerId
            (UnoStartedState stateData, CardPlayed playData) ->
              playResult stateData playData Nothing
            (UnoStartedState stateData, CardPlayedAndTurnBegan playData playerId) ->
              playResult stateData playData (Just playerId)
            _ -> EvolutionResult state
    }
  where
    initialResult
      :: AllowedTransition UnoTopology initialVertex 'Started
      => InitialData
      -> PlayerId
      -> EvolutionResult UnoTopology UnoState initialVertex (Either GameError Event)
    initialResult initialData playerId =
      EvolutionResult $
        UnoStartedState $
          StateData
            { topCard = firstCard initialData
            , currentPlayer = init playerId $ players initialData
            }

    playResult
      :: StateData
      -> PlayData
      -> Maybe PlayerId
      -> EvolutionResult UnoTopology UnoState 'Started (Either GameError Event)
    playResult stateData playData maybePlayerId =
      EvolutionResult $
        UnoStartedState $
          StateData
            { topCard = card playData
            , currentPlayer = maybe Prelude.id set maybePlayerId $ currentPlayer stateData
            }
