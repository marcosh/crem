{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag--Wall-missed-specialisations
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
-- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag--Wmissing-deriving-strategies
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}

module CRM.Example.LockDoor where

import CRM.BaseMachine
import CRM.Topology
import "singletons-base" Data.Singletons.Base.TH

$( singletons
    [d|
      data LockDoorVertex
        = IsLockOpen
        | IsLockClosed
        | IsLockLocked
        deriving stock (Eq, Show)

      lockDoorTopology :: Topology LockDoorVertex
      lockDoorTopology =
        Topology
          [ (IsLockOpen, [IsLockClosed])
          , (IsLockClosed, [IsLockOpen, IsLockLocked])
          , (IsLockLocked, [IsLockClosed])
          ]
      |]
 )

data LockDoorCommand
  = LockOpen
  | LockClose
  | LockLock
  | LockUnlock

data LockDoorEvent
  = LockNoOp
  | LockOpened
  | LockClosed
  | LockLocked
  | LockUnlocked
  deriving stock (Eq, Show)

lockDoorMachine :: SLockDoorVertex a -> BaseMachine LockDoorTopology LockDoorCommand LockDoorEvent
lockDoorMachine initialState =
  BaseMachineT
    { initialState = InitialState initialState
    , action = \case
        SIsLockOpen -> \case
          LockOpen -> pureResult LockNoOp SIsLockOpen
          LockClose -> pureResult LockClosed SIsLockClosed
          LockLock -> pureResult LockNoOp SIsLockOpen
          LockUnlock -> pureResult LockNoOp SIsLockOpen
        SIsLockClosed -> \case
          LockOpen -> pureResult LockOpened SIsLockOpen
          LockClose -> pureResult LockNoOp SIsLockClosed
          LockLock -> pureResult LockLocked SIsLockLocked
          LockUnlock -> pureResult LockNoOp SIsLockClosed
        SIsLockLocked -> \case
          LockOpen -> pureResult LockNoOp SIsLockLocked
          LockClose -> pureResult LockNoOp SIsLockLocked
          LockLock -> pureResult LockNoOp SIsLockLocked
          LockUnlock -> pureResult LockUnlocked SIsLockClosed
    }
