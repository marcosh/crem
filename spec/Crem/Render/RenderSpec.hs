{-# LANGUAGE OverloadedStrings #-}

module Crem.Render.RenderSpec where

import Crem.Example.LockDoor
import Crem.Example.OneState
import Crem.Example.Switch
import Crem.Topology (trivialTopology)
import Data.Functor.Identity
import Data.Singletons.Base.TH
import "base" Data.List (intersperse)
import "crem" Crem.Graph
import "crem" Crem.Render.Render
import "crem" Crem.StateMachine
import "hspec" Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "Render" $ do
    describe "renderMermaid" $ do
      it "should render correctly a graph" $ do
        renderGraph (Graph [(LT, LT), (LT, EQ), (LT, GT), (EQ, GT), (GT, LT)])
          `shouldBe` Mermaid
            ( mconcat $
                intersperse
                  "\n"
                  [ "LT"
                  , "EQ"
                  , "GT"
                  , "LT --> LT"
                  , "LT --> EQ"
                  , "LT --> GT"
                  , "EQ --> GT"
                  , "GT --> LT"
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
          `shouldBe` Mermaid "()"

      it "should render the basic switch machine" $ do
        renderUntypedGraph (machineAsGraph (Basic $ switchMachine SFalse @Identity))
          `shouldBe` Mermaid
            ( mconcat $
                intersperse
                  "\n"
                  [ "False"
                  , "True"
                  , "True --> False"
                  , "False --> True"
                  ]
            )

      it "should render the basic lockDoor machine" $ do
        renderUntypedGraph (machineAsGraph (Basic $ lockDoorMachine SIsLockClosed @Identity))
          `shouldBe` Mermaid
            ( mconcat $
                intersperse
                  "\n"
                  [ "IsLockOpen"
                  , "IsLockClosed"
                  , "IsLockLocked"
                  , "IsLockOpen --> IsLockClosed"
                  , "IsLockClosed --> IsLockOpen"
                  , "IsLockClosed --> IsLockLocked"
                  , "IsLockLocked --> IsLockClosed"
                  ]
            )
