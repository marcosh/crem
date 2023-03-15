module Crem.StateMachineSpec where

import Crem.Example.BooleanStateMachine (booleanStateMachine)
import Crem.Example.LockDoor
import Crem.Example.PlusOneUpToFour (plus1UpTo4)
import Crem.Example.Switch (switchMachine)
import "base" Control.Category qualified
import "base" Data.Functor.Identity (Identity (..))
import "base" Data.List (singleton)
import "crem" Crem.StateMachine
import "hspec" Test.Hspec (Expectation, Spec, describe, it, shouldBe)
import "profunctors" Data.Profunctor (rmap)
import "singletons-base" Data.Singletons.Base.TH

shouldOutput :: (Eq b, Show b) => Identity (b, c) -> b -> Expectation
shouldOutput (Identity (output, _)) expectedOutput = output `shouldBe` expectedOutput

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
            runOnce :: StateMachineT Identity LockDoorCommand LockDoorEvent
            runOnce = runIdentity $ snd <$> run (Basic $ lockDoorMachine SIsLockOpen @Identity) LockClose
          run runOnce LockLock `shouldOutput` LockLocked

        it "outputs Opened when it is locked and receive a Unlock and an Open command" $ do
          let
            runOnce :: StateMachineT Identity LockDoorCommand LockDoorEvent
            runOnce = runIdentity $ snd <$> run (Basic $ lockDoorMachine SIsLockLocked @Identity) LockUnlock
          run runOnce LockOpen `shouldOutput` LockOpened

      describe "boolean state machine" $ do
        it "outputs 1 when it is in a False state and receives a 0" $ do
          run (booleanStateMachine SFalse) 0 `shouldOutput` 1

        it "outputs 3 when it is in a False state and receives a 1" $ do
          run (booleanStateMachine SFalse) 1 `shouldOutput` 3

        it "outputs -1 when it is in a True state and receives a 0" $ do
          run (booleanStateMachine STrue) 0 `shouldOutput` (-1)

        it "outputs 5 when it is in a True state and receives a 1" $ do
          run (booleanStateMachine STrue) 1 `shouldOutput` 5

    -- describe "cotabulate and cosieve are each other inverses" $ do
    --   prop "cotabulate . cosieve = id" $ do
    --     forAll (arbitrary @Int) $ do
    --       \input ->
    --         run (booleanStateMachine SFalse) input
    --           `shouldHaveTheSameOutputAs` run (cotabulate . cosieve $ booleanStateMachine SFalse) input

    --   prop "cosieve . cotabulate = id" $ do
    --     let
    --       nonEmptyFunction :: NonEmpty Int -> Int
    --       nonEmptyFunction = sum
    --     forAll (fromList <$> (arbitrary @[Int] `suchThat` (not . null))) $ do
    --       \input ->
    --         nonEmptyFunction input
    --           `shouldBe` (cosieve . cotabulate @StateMachine $ nonEmptyFunction) input

    describe "Feedback constructor runs correctly" $ do
      describe "with the plus1UpTo4 machine" $ do
        let
          echo :: StateMachine a [a]
          echo = rmap singleton Control.Category.id

        it "runs correctly on a single input" $ do
          run (Feedback echo plus1UpTo4) 1 `shouldOutput` [1, 2, 3, 4, 5]
          run (Feedback echo plus1UpTo4) 5 `shouldOutput` [5]

        it "processes correctly multiple inputs" $ do
          runMultiple (Feedback echo plus1UpTo4) [1, 1] `shouldOutput` [1, 2, 3, 4, 5, 1, 2, 3, 4, 5]
