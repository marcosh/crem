module Crem.Render.RenderFlowSpec where

import "crem" Crem.Example.LockDoor (SLockDoorVertex (..), lockDoorMachine)
import "crem" Crem.Example.RiskManager.Application (riskApplication)
import "crem" Crem.Render.Render (MachineLabel (..), Mermaid (..), baseMachineAsGraph, renderLabelledGraph)
import "crem" Crem.Render.RenderFlow (TreeMetadata (..), renderFlow)
import "crem" Crem.StateMachine (StateMachineT (..), stateless)
import "base" Data.Functor.Identity (Identity)
import "base" Data.List (singleton)
import "hspec" Test.Hspec

spec :: Spec
spec =
  describe "RenderFlow" $ do
    describe "renderFlow" $ do
      it "renders correctly a base machine" $ do
        renderFlow @Identity (LeafLabel "lockMachine") (Basic $ lockDoorMachine SIsLockClosed)
          `shouldBe` Right
            ( Mermaid "state lockMachine {"
                <> ( renderLabelledGraph "lockMachine" . baseMachineAsGraph @_ @_ @_ @_ @Identity $
                      lockDoorMachine SIsLockClosed
                   )
                <> Mermaid "}"
            , MachineLabel "lockMachine"
            , MachineLabel "lockMachine"
            )

      it "renders correctly a Compose machine" $ do
        renderFlow
          @Identity
          (BinaryLabel (LeafLabel "show") (LeafLabel "length"))
          ( Compose
              (stateless $ show @Int)
              (stateless length)
          )
          `shouldBe` Right
            ( Mermaid "state show {\nshow_()\n}\nstate length {\nlength_()\n}\nshow --> length"
            , MachineLabel "show"
            , MachineLabel "length"
            )

      it "renders correctly a Parallel machine" $ do
        renderFlow
          @Identity
          (BinaryLabel (LeafLabel "foo") (LeafLabel "bar"))
          ( Parallel
              (stateless $ show @Int)
              (stateless $ length @[] @String)
          )
          `shouldBe` Right
            ( Mermaid "state foo {\nfoo_()\n}\nstate bar {\nbar_()\n}\nstate fork_foobar <<fork>>\nstate join_foobar <<join>>\nfork_foobar --> foo\nfork_foobar --> bar\nfoo --> join_foobar\nbar --> join_foobar"
            , MachineLabel "fork_foobar"
            , MachineLabel "join_foobar"
            )

      it "renders correctly an Alternative machine" $ do
        renderFlow
          @Identity
          (BinaryLabel (LeafLabel "foo") (LeafLabel "bar"))
          ( Alternative
              (stateless $ show @Int)
              (stateless $ length @[] @String)
          )
          `shouldBe` Right
            ( Mermaid "state foo {\nfoo_()\n}\nstate bar {\nbar_()\n}\nstate fork_choice_foobar <<choice>>\nstate join_choice_foobar <<choice>>\nfork_choice_foobar --> foo\nfork_choice_foobar --> bar\nfoo --> join_choice_foobar\nbar --> join_choice_foobar"
            , MachineLabel "fork_choice_foobar"
            , MachineLabel "join_choice_foobar"
            )

      it "renders correctly a Feedback machine" $ do
        renderFlow
          @Identity
          (BinaryLabel (LeafLabel "foo") (LeafLabel "bar"))
          ( Feedback
              (stateless $ singleton @Int)
              (stateless $ singleton @Int)
          )
          `shouldBe` Right
            ( Mermaid "state foo {\nfoo_()\n}\nstate bar {\nbar_()\n}\nfoo --> bar: []\nbar --> foo: []"
            , MachineLabel "foo"
            , MachineLabel "foo"
            )

      it "renders correctly a Kleisli machine" $ do
        renderFlow
          @Identity
          (BinaryLabel (LeafLabel "show") (LeafLabel "length"))
          ( Kleisli
              (stateless $ singleton @Int)
              (stateless $ singleton @Int)
          )
          `shouldBe` Right
            ( Mermaid "state show {\nshow_()\n}\nstate length {\nlength_()\n}\nshow --> length: []"
            , MachineLabel "show"
            , MachineLabel "length"
            )

      it "renders correctly the RiskManager machine" $ do
        renderFlow
          @Identity
          ( BinaryLabel
              ( BinaryLabel
                  ( BinaryLabel
                      (LeafLabel "aggregate")
                      (LeafLabel "policy")
                  )
                  (LeafLabel "projection")
              )
              (LeafLabel "mconcat")
          )
          riskApplication
          `shouldBe` Right
            ( Mermaid "state aggregate {\naggregate_NoDataVertex\naggregate_CollectedUserDataVertex\naggregate_CollectedLoanDetailsFirstVertex\naggregate_ReceivedCreditBureauDataFirstVertex\naggregate_CollectedAllDataVertex\naggregate_NoDataVertex --> aggregate_CollectedUserDataVertex\naggregate_CollectedUserDataVertex --> aggregate_CollectedLoanDetailsFirstVertex\naggregate_CollectedUserDataVertex --> aggregate_ReceivedCreditBureauDataFirstVertex\naggregate_CollectedLoanDetailsFirstVertex --> aggregate_CollectedAllDataVertex\naggregate_ReceivedCreditBureauDataFirstVertex --> aggregate_CollectedAllDataVertex\n}\nstate policy {\npolicy_()\n}\naggregate --> policy: []\npolicy --> aggregate: []\nstate projection {\nprojection_SingleProjectionVertex\n}\naggregate --> projection: []\nstate mconcat {\nmconcat_()\n}\nprojection --> mconcat"
            , "aggregate"
            , "mconcat"
            )
