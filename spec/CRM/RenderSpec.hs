module CRM.RenderSpec where

import CRM.Example.LockDoor
import CRM.Example.OneState
import CRM.Example.Switch
import "crm" CRM.Graph
import "crm" CRM.Render
import "crm" CRM.StateMachine
import CRM.Topology (trivialTopology)
import Data.Functor.Identity
import Data.Singletons.Base.TH
import "text" Data.Text as Text (unlines)
import "hspec" Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "Render" $ do
    describe "renderMermaid" $ do
      it "should render correctly a graph" $ do
        renderGraph (Graph [(1 :: Int, 1), (1, 2), (1, 3), (2, 3), (3, 1)])
          `shouldBe` Mermaid
            ( Text.unlines
                [ "1 --> 1"
                , "1 --> 2"
                , "1 --> 3"
                , "2 --> 3"
                , "3 --> 1"
                ]
            )

    describe "topologyAsGraph" $ do
      it "should render the topology with a single vertex" $ do
        topologyAsGraph (trivialTopology @())
          `shouldBe` Graph []

      it "should render the switch topology" $ do
        topologyAsGraph switchTopology
          `shouldBe` Graph
            [ (True, False)
            , (False, True)
            ]

      it "should render the lockDoor topology" $ do
        topologyAsGraph lockDoorTopology
          `shouldBe` Graph
            [ (IsLockOpen, IsLockClosed)
            , (IsLockClosed, IsLockOpen)
            , (IsLockClosed, IsLockLocked)
            , (IsLockLocked, IsLockClosed)
            ]

    describe "baseMachineAsGraph" $ do
      it "should render the machine with a single vertex" $ do
        baseMachineAsGraph (oneVertexMachine @Identity)
          `shouldBe` Graph []

      it "should render the switch machine" $ do
        baseMachineAsGraph (switchMachine SFalse @Identity)
          `shouldBe` Graph
            [ (True, False)
            , (False, True)
            ]

      it "should render the lockDoor machine" $ do
        baseMachineAsGraph (lockDoorMachine SIsLockClosed @Identity)
          `shouldBe` Graph
            [ (IsLockOpen, IsLockClosed)
            , (IsLockClosed, IsLockOpen)
            , (IsLockClosed, IsLockLocked)
            , (IsLockLocked, IsLockClosed)
            ]

    describe "machineAsGraph" $ do
      it "should render the basic machine with a single vertex" $ do
        renderUntypedGraph (machineAsGraph (Basic $ oneVertexMachine @Identity))
          `shouldBe` Mermaid
            ( Text.unlines
                []
            )

      it "should render the basic switch machine" $ do
        renderUntypedGraph (machineAsGraph (Basic $ switchMachine SFalse @Identity))
          `shouldBe` Mermaid
            ( Text.unlines
                [ "True --> False"
                , "False --> True"
                ]
            )

      it "should render the basic lockDoor machine" $ do
        renderUntypedGraph (machineAsGraph (Basic $ lockDoorMachine SIsLockClosed @Identity))
          `shouldBe` Mermaid
            ( Text.unlines
                [ "IsLockOpen --> IsLockClosed"
                , "IsLockClosed --> IsLockOpen"
                , "IsLockClosed --> IsLockLocked"
                , "IsLockLocked --> IsLockClosed"
                ]
            )
