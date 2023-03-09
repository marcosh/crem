{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag--Wmissing-deriving-strategies
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
-- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag--Wunticked-promoted-constructors
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
-- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag--Wunused-type-patterns
{-# OPTIONS_GHC -Wno-unused-type-patterns #-}

module Crem.Example.TheHobbit where

import Crem.BaseMachine
import Crem.Topology
import "base" Data.Semigroup
import "singletons-base" Data.Singletons.Base.TH

data HobbitCommand
  = GoEast
  | GoWest
  | GoNorth
  | GoSouth
  | Wait
  | GetKey
  | UnlockDoor
  deriving stock (Eq, Read, Show)

newtype HobbitMessage = HobbitMessage {getMessage :: String}
  deriving (Semigroup) via (Last HobbitMessage)

instance Monoid HobbitMessage where
  mempty = HobbitMessage ""

$( singletons
    [d|
      data HobbitVertex
        = TunnelLikeHall
        | Lonelands
        | TrollsClearing
        | Rivendell
        | MistyMountain
        | TrollsPath
        | TrollsCave
        deriving stock (Eq, Show, Enum, Bounded)

      hobbitTopology :: Topology HobbitVertex
      hobbitTopology =
        Topology
          [ (TunnelLikeHall, [Lonelands])
          , (Lonelands, [TunnelLikeHall, TrollsClearing])
          , (TrollsClearing, [Rivendell, TrollsPath])
          , (Rivendell, [TrollsClearing, MistyMountain])
          , (MistyMountain, [Rivendell])
          , (TrollsPath, [TrollsClearing, TrollsCave])
          , (TrollsCave, [TrollsPath])
          ]
      |]
 )

data KeyState
  = NoKey
  | DayDawned
  | GotKey
  | DoorUnlocked
  deriving stock (Eq)

data HobbitState (vertex :: HobbitVertex) where
  TunnelLikeHallState :: HobbitState 'TunnelLikeHall
  LonelandsState :: HobbitState 'Lonelands
  TrollsClearingState :: KeyState -> HobbitState 'TrollsClearing
  RivendellState :: KeyState -> HobbitState 'Rivendell
  MistyMountainState :: KeyState -> HobbitState 'MistyMountain
  TrollsPathState :: KeyState -> HobbitState 'TrollsPath
  TrollsCaveState :: HobbitState 'TrollsCave

stateMessage :: HobbitState vertex -> HobbitMessage
stateMessage TunnelLikeHallState =
  HobbitMessage
    "You are in a tunnel-like hall.\n\
    \You can only go east to the Lonelands"
stateMessage LonelandsState =
  HobbitMessage
    "You are in the lonelands.\n\
    \You can either go west to a tunnel-like hall\n\
    \or go east to the Trolls clearing"
stateMessage (TrollsClearingState keyState) =
  if keyState == DayDawned
    then
      HobbitMessage
        "You are in the Trolls clearing.\n\
        \You could go north to the Trolls path,\n\
        \you can go east to Rivendell\n\
        \or you could get the key for the TrollsCave"
    else
      HobbitMessage
        "You are in the Trolls clearing.\n\
        \You could go north to the Trolls path,\n\
        \you can go east to Rivendell"
stateMessage (RivendellState _) =
  HobbitMessage
    "You are in Rivendell.\n\
    \You could either go west to the Trolls clearing\n\
    \or go east to the Misty mountains\n"
stateMessage (MistyMountainState _) =
  HobbitMessage
    "You are in the Misty mountains.\n\
    \You can only go east to Rivendell"
stateMessage (TrollsPathState keyState) =
  case keyState of
    NoKey ->
      HobbitMessage
        "You are in the Trolls path.\n\
        \You can go south to the Trolls clearing\n\
        \or you can wait a bit"
    DayDawned ->
      HobbitMessage
        "You are in the Trolls path.\n\
        \You can go south to the Trolls clearing\n\
        \or you can wait some more"
    GotKey ->
      HobbitMessage
        "You are in the Trolls path.\n\
        \You can go south to the Trolls clearing,\n\
        \you can unlock the door to the Trolls cave\n\
        \or you can wait some more"
    DoorUnlocked ->
      HobbitMessage
        "You are in the Trolls path.\n\
        \You can go south to the Trolls clearing\n\
        \or you can go north to the Trolls cave"
stateMessage TrollsCaveState =
  HobbitMessage
    "Welcome to the Trolls cave!\n\
    \Now you can go back south to the Trolls path"

hobbitResult
  :: (Applicative m, AllowedTransition HobbitTopology initialVertex finalVertex)
  => HobbitState finalVertex
  -> ActionResult m HobbitTopology HobbitState initialVertex HobbitMessage
hobbitResult hobbitState = pureResult (stateMessage hobbitState) hobbitState

hobbitMachine :: HobbitState vertex -> BaseMachine HobbitTopology HobbitCommand HobbitMessage
hobbitMachine initialState =
  BaseMachineT
    { initialState = InitialState initialState
    , action = \case
        TunnelLikeHallState -> \case
          GoEast -> hobbitResult LonelandsState
          _ -> hobbitResult TunnelLikeHallState
        LonelandsState -> \case
          GoEast -> hobbitResult $ TrollsClearingState NoKey
          GoWest -> hobbitResult TunnelLikeHallState
          _ -> hobbitResult LonelandsState
        TrollsClearingState keyState -> \case
          GoEast -> hobbitResult $ RivendellState NoKey
          GoNorth -> hobbitResult $ TrollsPathState keyState
          GetKey ->
            if keyState == DayDawned
              then hobbitResult $ TrollsClearingState GotKey
              else hobbitResult $ TrollsClearingState keyState
          _ -> hobbitResult $ TrollsClearingState keyState
        RivendellState keyState -> \case
          GoEast -> hobbitResult $ MistyMountainState keyState
          GoWest -> hobbitResult $ TrollsClearingState keyState
          _ -> hobbitResult $ RivendellState keyState
        MistyMountainState keyState -> \case
          GoWest -> hobbitResult $ RivendellState keyState
          _ -> hobbitResult $ MistyMountainState keyState
        TrollsPathState keyState -> \case
          GoSouth -> hobbitResult $ TrollsClearingState keyState
          GoNorth ->
            if keyState == DoorUnlocked
              then hobbitResult TrollsCaveState
              else hobbitResult $ TrollsPathState keyState
          Wait ->
            if keyState == NoKey
              then hobbitResult $ TrollsPathState DayDawned
              else hobbitResult $ TrollsPathState keyState
          UnlockDoor ->
            if keyState == GotKey
              then hobbitResult $ TrollsPathState DoorUnlocked
              else hobbitResult $ TrollsPathState keyState
          _ -> hobbitResult (TrollsPathState keyState)
        TrollsCaveState -> \case
          GoSouth -> hobbitResult $ TrollsPathState DoorUnlocked
          _ -> hobbitResult TrollsCaveState
    }