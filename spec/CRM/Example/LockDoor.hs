{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag--Wall-missed-specialisations
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
-- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag--Wmissing-deriving-strategies
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}

module CRM.Example.LockDoor where

import "crm" CRM.BaseMachine
import "crm" CRM.Topology
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
  BaseMachine
    { initialState = InitialState initialState
    , action = \case
        SIsLockOpen -> \case
          LockOpen -> ActionResult SIsLockOpen LockNoOp
          LockClose -> ActionResult SIsLockClosed LockClosed
          LockLock -> ActionResult SIsLockOpen LockNoOp
          LockUnlock -> ActionResult SIsLockOpen LockNoOp
        SIsLockClosed -> \case
          LockOpen -> ActionResult SIsLockOpen LockOpened
          LockClose -> ActionResult SIsLockClosed LockNoOp
          LockLock -> ActionResult SIsLockLocked LockLocked
          LockUnlock -> ActionResult SIsLockClosed LockNoOp
        SIsLockLocked -> \case
          LockOpen -> ActionResult SIsLockLocked LockNoOp
          LockClose -> ActionResult SIsLockLocked LockNoOp
          LockLock -> ActionResult SIsLockLocked LockNoOp
          LockUnlock -> ActionResult SIsLockClosed LockUnlocked
    }