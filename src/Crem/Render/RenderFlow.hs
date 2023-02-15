{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Rendering just the state space of a state machine might be sometimes
-- limiting.
--
-- Given that the `StateMachineT` data type encodes a lot of information on the
-- structure of a machine, we can actually use it to render a more informative
-- flow.
module Crem.Render.RenderFlow where

import Crem.Render.Render
import Crem.StateMachine

-- | A tree-like structure which could be used to attach metadata to any
-- similar tree-like structure with only leaves and nodes with exactly two
-- child.
data TreeMetadata a
  = LeafLabel a
  | BinaryLabel (TreeMetadata a) (TreeMetadata a)
  deriving stock (Show)

-- | Given a `StateMachineT` and a `TreeMetadata` of @MachineLabel@s, we can
-- create a flow representation of our machine.
--
-- For every leaf it will render the state space, while for every other node,
-- it will render the flow between the composed machines.
--
-- More details available in [\/docs\/how-to-render-a-machine.md](https://github.com/tweag/crem/tree/main/docs/how-to-render-a-machine.md)
renderFlow :: TreeMetadata MachineLabel -> StateMachineT m input output -> Either String (Mermaid, MachineLabel, MachineLabel)
renderFlow (LeafLabel label) (Basic machine) =
  Right
    ( Mermaid ("state " <> getLabel label <> " {")
        <> renderLabelledGraph label (baseMachineAsGraph machine)
        <> Mermaid "}"
    , label
    , label
    )
renderFlow (BinaryLabel leftLabels rightLabels) (Sequential machine1 machine2) = do
  (leftMermaid, leftLabelIn, leftLabelOut) <- renderFlow leftLabels machine1
  (rightMermaid, rightLabelIn, rightLabelOut) <- renderFlow rightLabels machine2
  Right
    ( leftMermaid
        <> rightMermaid
        <> Mermaid (getLabel leftLabelOut <> " --> " <> getLabel rightLabelIn)
    , leftLabelIn
    , rightLabelOut
    )
renderFlow (BinaryLabel upperLabels lowerLabels) (Parallel machine1 machine2) = do
  (upperMermaid, upperLabelIn, upperLabelOut) <- renderFlow upperLabels machine1
  (lowerMermaid, lowerLabelIn, lowerLabelOut) <- renderFlow lowerLabels machine2
  let
    inLabel = "fork_" <> getLabel upperLabelIn <> getLabel lowerLabelIn
    outLabel = "join_" <> getLabel upperLabelOut <> getLabel lowerLabelOut
  Right
    ( upperMermaid
        <> lowerMermaid
        <> Mermaid ("state " <> inLabel <> " <<fork>>")
        <> Mermaid ("state " <> outLabel <> " <<join>>")
        <> Mermaid (inLabel <> " --> " <> getLabel upperLabelIn)
        <> Mermaid (inLabel <> " --> " <> getLabel lowerLabelIn)
        <> Mermaid (getLabel upperLabelOut <> " --> " <> outLabel)
        <> Mermaid (getLabel lowerLabelOut <> " --> " <> outLabel)
    , MachineLabel inLabel
    , MachineLabel outLabel
    )
renderFlow (BinaryLabel upperLabels lowerLabels) (Alternative machine1 machine2) = do
  (upperMermaid, upperLabelIn, upperLabelOut) <- renderFlow upperLabels machine1
  (lowerMermaid, lowerLabelIn, lowerLabelOut) <- renderFlow lowerLabels machine2
  let
    inLabel = "fork_choice_" <> getLabel upperLabelIn <> getLabel lowerLabelIn
    outLabel = "join_choice_" <> getLabel upperLabelOut <> getLabel lowerLabelOut
  Right
    ( upperMermaid
        <> lowerMermaid
        <> Mermaid ("state " <> inLabel <> " <<choice>>")
        <> Mermaid ("state " <> outLabel <> " <<choice>>")
        <> Mermaid (inLabel <> " --> " <> getLabel upperLabelIn)
        <> Mermaid (inLabel <> " --> " <> getLabel lowerLabelIn)
        <> Mermaid (getLabel upperLabelOut <> " --> " <> outLabel)
        <> Mermaid (getLabel lowerLabelOut <> " --> " <> outLabel)
    , MachineLabel inLabel
    , MachineLabel outLabel
    )
renderFlow (BinaryLabel forwardLabels backwardsLabels) (Feedback machine1 machine2) = do
  (forwardMermaid, forwardLabelIn, forwardLabelOut) <- renderFlow forwardLabels machine1
  (backwardMermaid, backawardLabelIn, backwardLabelOut) <- renderFlow backwardsLabels machine2
  Right
    ( forwardMermaid
        <> backwardMermaid
        <> Mermaid (getLabel forwardLabelOut <> " --> " <> getLabel backawardLabelIn <> ": []")
        <> Mermaid (getLabel backwardLabelOut <> " --> " <> getLabel forwardLabelIn <> ": []")
    , forwardLabelIn
    , forwardLabelOut
    )
renderFlow (BinaryLabel leftLabels rightLabels) (Kleisli machine1 machine2) = do
  (leftMermaid, leftLabelIn, leftLabelOut) <- renderFlow leftLabels machine1
  (rightMermaid, rightLabelIn, rightLabelOut) <- renderFlow rightLabels machine2
  Right
    ( leftMermaid
        <> rightMermaid
        <> Mermaid (getLabel leftLabelOut <> " --> " <> getLabel rightLabelIn <> ": []")
    , leftLabelIn
    , rightLabelOut
    )
renderFlow labels _ = Left $ "Labels structure " <> show labels <> " does not match machine structure" -- TODO: this sucks
