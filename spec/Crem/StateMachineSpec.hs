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
import "machines" Data.Machine (runT, source, (~>))
import "machines" Data.Machine.Process (autoT)
import "machines" Data.Machine.Process qualified
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

    describe "AutomatonM instance" $ do
      describe "autoT converts StateMachineT to ProcessT" $ do
        it "processes a single input through a stateless machine" $ do
          let machine = stateless (+ 1) :: StateMachineT Identity Int Int
          let process = autoT machine
          let result = runIdentity $ runT $ source [5] ~> process
          result `shouldBe` [6]

        it "processes multiple inputs through a stateless machine" $ do
          let machine = stateless (* 2) :: StateMachineT Identity Int Int
          let process = autoT machine
          let result = runIdentity $ runT $ source [1, 2, 3, 4] ~> process
          result `shouldBe` [2, 4, 6, 8]

        it "processes inputs through a stateful machine (switch)" $ do
          let machine = Basic $ switchMachine SFalse :: StateMachineT Identity () ()
          let process = autoT machine
          let result = runIdentity $ runT $ source [(), (), ()] ~> process
          result `shouldBe` [(), (), ()]

        it "maintains state across inputs (lock door)" $ do
          let machine = Basic $ lockDoorMachine SIsLockClosed :: StateMachineT Identity LockDoorCommand LockDoorEvent
          let process = autoT machine :: Data.Machine.Process.ProcessT Identity LockDoorCommand LockDoorEvent
          let result = runIdentity $ runT $ source [LockLock, LockUnlock, LockOpen] ~> process
          result `shouldBe` [LockLocked, LockUnlocked, LockOpened]

        it "works with sequential composition" $ do
          let machine1 = stateless (+ 1) :: StateMachineT Identity Int Int
          let machine2 = stateless (* 2) :: StateMachineT Identity Int Int
          let composed = machine2 Control.Category.. machine1 -- Category composition: right-to-left
          let process = autoT composed
          let result = runIdentity $ runT $ source [1, 2, 3] ~> process
          result `shouldBe` [4, 6, 8]

        it "works with parallel composition" $ do
          let machine1 = stateless (+ 1) :: StateMachineT Identity Int Int
          let machine2 = stateless (* 2) :: StateMachineT Identity Int Int
          let parallel = Parallel machine1 machine2
          let process = autoT parallel
          let result = runIdentity $ runT $ source [(1, 10), (2, 20), (3, 30)] ~> process
          result `shouldBe` [(2, 20), (3, 40), (4, 60)]

        it "works with alternative composition (left)" $ do
          let machine1 = stateless (+ 1) :: StateMachineT Identity Int Int
          let machine2 = stateless (* 2) :: StateMachineT Identity Int Int
          let alt = Alternative machine1 machine2
          let process = autoT alt
          let result = runIdentity $ runT $ source [Left 1, Left 2, Left 3] ~> process
          result `shouldBe` [Left 2, Left 3, Left 4]

        it "works with alternative composition (right)" $ do
          let machine1 = stateless (+ 1) :: StateMachineT Identity Int Int
          let machine2 = stateless (* 2) :: StateMachineT Identity Int Int
          let alt = Alternative machine1 machine2
          let process = autoT alt
          let result = runIdentity $ runT $ source [Right 1, Right 2, Right 3] ~> process
          result `shouldBe` [Right 2, Right 4, Right 6]

        it "works with mixed alternative composition" $ do
          let machine1 = stateless (+ 1) :: StateMachineT Identity Int Int
          let machine2 = stateless (* 2) :: StateMachineT Identity Int Int
          let alt = Alternative machine1 machine2
          let process = autoT alt
          let result = runIdentity $ runT $ source [Left 1, Right 5, Left 2] ~> process
          result `shouldBe` [Left 2, Right 10, Left 3]

        it "handles boolean state machine correctly" $ do
          let machine = booleanStateMachine SFalse :: StateMachineT Identity Int Int
          let process = autoT machine :: Data.Machine.Process.ProcessT Identity Int Int
          let result = runIdentity $ runT $ source [0, 1, 0] ~> process
          -- Starting at SFalse: 0 (even) -> 1, stay SFalse; 1 (odd) -> 3, move to STrue; 0 (even) -> -1, stay STrue
          result `shouldBe` [1, 3, -1]
