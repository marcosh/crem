module CRM.StateMachineSpec where

import CRM.Example.LockDoor
import CRM.Example.Switch (switchMachine)
import "crm" CRM.StateMachine
import "singletons-base" Data.Singletons.Base.TH
import "hspec" Test.Hspec (Expectation, Spec, describe, it, shouldBe)

shouldOutput :: (Eq b, Show b) => (b, StateMachine a b) -> b -> Expectation
shouldOutput (output, _) expectedOutput = output `shouldBe` expectedOutput

spec :: Spec
spec =
  describe "StateMachine" $ do
    describe "run" $ do
      describe "switch machine" $ do
        it "outputs () when the state is False" $ do
          run (Basic $ switchMachine SFalse) () `shouldOutput` ()

        it "outputs () when the state is True" $ do
          run (Basic $ switchMachine STrue) () `shouldOutput` ()

      describe "lock door machine" $ do
        it "outputs Opened when it is closed and receive an Open command" $ do
          run (Basic $ lockDoorMachine SIsLockClosed) LockOpen `shouldOutput` LockOpened

        it "outputs NoOp when it is closed and receive a Close command" $ do
          run (Basic $ lockDoorMachine SIsLockClosed) LockClose `shouldOutput` LockNoOp

        it "outputs Locked when it is closed and receive a Lock command" $ do
          run (Basic $ lockDoorMachine SIsLockClosed) LockLock `shouldOutput` LockLocked

        it "outputs NoOp when it is closed and receive an Unlock command" $ do
          run (Basic $ lockDoorMachine SIsLockClosed) LockUnlock `shouldOutput` LockNoOp

        it "outputs NoOp when it is open and receive an Open command" $ do
          run (Basic $ lockDoorMachine SIsLockOpen) LockOpen `shouldOutput` LockNoOp

        it "outputs Closed when it is open and receive a Close command" $ do
          run (Basic $ lockDoorMachine SIsLockOpen) LockClose `shouldOutput` LockClosed

        it "outputs NoOp when it is open and receive a Lock command" $ do
          run (Basic $ lockDoorMachine SIsLockOpen) LockLock `shouldOutput` LockNoOp

        it "outputs NoOp when it is open and receive an Unlock command" $ do
          run (Basic $ lockDoorMachine SIsLockOpen) LockUnlock `shouldOutput` LockNoOp

        it "outputs NoOp when it is locked and receive an Open command" $ do
          run (Basic $ lockDoorMachine SIsLockLocked) LockOpen `shouldOutput` LockNoOp

        it "outputs NoOp when it is locked and receive a Close command" $ do
          run (Basic $ lockDoorMachine SIsLockLocked) LockClose `shouldOutput` LockNoOp

        it "outputs NoOp when it is locked and receive a Lock command" $ do
          run (Basic $ lockDoorMachine SIsLockLocked) LockLock `shouldOutput` LockNoOp

        it "outputs Unlocked when it is locked and receive an Unlock command" $ do
          run (Basic $ lockDoorMachine SIsLockLocked) LockUnlock `shouldOutput` LockUnlocked

        it "outputs Locked when it is open and receive a Close and a Lock command" $ do
          let
            runOnce = snd $ run (Basic $ lockDoorMachine SIsLockOpen) LockClose
          run runOnce LockLock `shouldOutput` LockLocked

        it "outputs Opened when it is locked and receive a Unlock and an Open command" $ do
          let
            runOnce = snd $ run (Basic $ lockDoorMachine SIsLockLocked) LockUnlock
          run runOnce LockOpen `shouldOutput` LockOpened
