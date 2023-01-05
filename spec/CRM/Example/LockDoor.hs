{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module CRM.Example.LockDoor where

import "crm" CRM.BaseMachine
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
        MkTopology
          [ (IsLockOpen, [IsLockOpen, IsLockClosed])
          , (IsLockClosed, [IsLockClosed, IsLockOpen, IsLockLocked])
          , (IsLockLocked, [IsLockLocked, IsLockClosed])
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

lockDoorMachine :: StateMachine LockDoorTopology LockDoorCommand LockDoorEvent
lockDoorMachine =
  MkStateMachine
    { initialState = MkInitialState SIsLockClosed
    , action = \case
        SIsLockOpen -> \case
          LockOpen -> MkActionResult SIsLockOpen LockNoOp
          LockClose -> MkActionResult SIsLockClosed LockClosed
          LockLock -> MkActionResult SIsLockOpen LockNoOp
          LockUnlock -> MkActionResult SIsLockOpen LockNoOp
        SIsLockClosed -> \case
          LockOpen -> MkActionResult SIsLockOpen LockOpened
          LockClose -> MkActionResult SIsLockClosed LockNoOp
          LockLock -> MkActionResult SIsLockLocked LockLocked
          LockUnlock -> MkActionResult SIsLockClosed LockNoOp
        SIsLockLocked -> \case
          LockOpen -> MkActionResult SIsLockLocked LockNoOp
          LockClose -> MkActionResult SIsLockLocked LockNoOp
          LockLock -> MkActionResult SIsLockLocked LockNoOp
          LockUnlock -> MkActionResult SIsLockClosed LockUnlocked
    }