module CRM.RenderSpec where

import CRM.Example.LockDoor
import CRM.Example.OneState
import CRM.Example.Switch
import "crm" CRM.Render
import "text" Data.Text as Text (unlines)
import "hspec" Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "Render" $ do
    describe "renderMermaid" $ do
      it "should render correctly a graph" $ do
        renderMermaid (Graph [(1 :: Int, 1), (1, 2), (1, 3), (2, 3), (3, 1)])
          `shouldBe` Text.unlines
            [ "stateDiagram-v2"
            , "1 --> 1"
            , "1 --> 2"
            , "1 --> 3"
            , "2 --> 3"
            , "3 --> 1"
            ]

    describe "topologyAsGraph" $ do
      it "should render the topology with a single vertex" $ do
        topologyAsGraph singleVertexTopology
          `shouldBe` Graph
            [ ((), ())
            ]

      it "should render the switch topology" $ do
        topologyAsGraph switchTopology
          `shouldBe` Graph
            [ (True, False)
            , (False, True)
            ]

      it "should render the lockDoor topology" $ do
        topologyAsGraph lockDoorTopology
          `shouldBe` Graph
            [ (IsLockOpen, IsLockOpen)
            , (IsLockOpen, IsLockClosed)
            , (IsLockClosed, IsLockClosed)
            , (IsLockClosed, IsLockOpen)
            , (IsLockClosed, IsLockLocked)
            , (IsLockLocked, IsLockLocked)
            , (IsLockLocked, IsLockClosed)
            ]

    describe "baseMachineAsGraph" $ do
      it "should render the machine with a single vertex" $ do
        baseMachineAsGraph oneVertexMachine
          `shouldBe` Graph
            [ ((), ())
            ]

      it "should render the switch machine" $ do
        baseMachineAsGraph switchMachine
          `shouldBe` Graph
            [ (True, False)
            , (False, True)
            ]

      it "should render the lockDoor machine" $ do
        baseMachineAsGraph lockDoorMachine
          `shouldBe` Graph
            [ (IsLockOpen, IsLockOpen)
            , (IsLockOpen, IsLockClosed)
            , (IsLockClosed, IsLockClosed)
            , (IsLockClosed, IsLockOpen)
            , (IsLockClosed, IsLockLocked)
            , (IsLockLocked, IsLockLocked)
            , (IsLockLocked, IsLockClosed)
            ]
