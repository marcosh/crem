module CRM.RenderSpec where

import CRM.Example.LockDoor
import CRM.Example.OneState
import CRM.Example.Switch
import "crm" CRM.Render
import "crm" CRM.StateMachine
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

    describe "machineAsGraph" $ do
      it "should render the basic machine with a single vertex" $ do
        renderUntypedMermaid (machineAsGraph (Basic oneVertexMachine))
          `shouldBe` Text.unlines
            [ "stateDiagram-v2"
            , "() --> ()"
            ]

      it "should render the basic switch machine" $ do
        renderUntypedMermaid (machineAsGraph (Basic switchMachine))
          `shouldBe` Text.unlines
            [ "stateDiagram-v2"
            , "True --> False"
            , "False --> True"
            ]

      it "should render the basic lockDoor machine" $ do
        renderUntypedMermaid (machineAsGraph (Basic lockDoorMachine))
          `shouldBe` Text.unlines
            [ "stateDiagram-v2"
            , "IsLockOpen --> IsLockOpen"
            , "IsLockOpen --> IsLockClosed"
            , "IsLockClosed --> IsLockClosed"
            , "IsLockClosed --> IsLockOpen"
            , "IsLockClosed --> IsLockLocked"
            , "IsLockLocked --> IsLockLocked"
            , "IsLockLocked --> IsLockClosed"
            ]
