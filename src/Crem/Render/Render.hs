{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Once you defined a `StateMachineT`, you can render its topology as a
-- directed graph using a [Mermaid](https://mermaid.js.org/#/) state diagram
module Crem.Render.Render where

import Crem.BaseMachine
import Crem.Graph
import Crem.Render.RenderableVertices
import Crem.StateMachine
import Crem.Topology
import "base" Data.List (intersperse)
import "singletons-base" Data.Singletons (Demote, SingI, SingKind, demote)
import "base" Data.String (IsString)
import "text" Data.Text (Text, null, pack)
import Prelude hiding (null)

-- | `Mermaid` is just a @newtype@ around @Text@ to specialize it to Mermaid
-- diagrams
newtype Mermaid = Mermaid {getText :: Text}
  deriving newtype (Eq, Show)

-- | Notice that we joining two non-empty mermaid diagrams, a newline will be
-- added
instance Semigroup Mermaid where
  (<>) :: Mermaid -> Mermaid -> Mermaid
  (Mermaid "") <> m = m
  m <> (Mermaid "") = m
  (Mermaid t1) <> (Mermaid t2) = Mermaid (t1 <> "\n" <> t2)

-- | A `MachineLabel` is just a newtype around `Text` to represents label which
-- will be attached to every leaf of the tree defined by the constructors of
-- `StateMachineT`
newtype MachineLabel = MachineLabel {getLabel :: Text}
  deriving newtype (Eq, Show, IsString)

-- | We can render a `Graph` as a Mermaid state diagram
renderStateDiagram :: (RenderableVertices a, Show a) => Graph a -> Mermaid
renderStateDiagram graph =
  Mermaid "stateDiagram-v2\n" <> renderGraph graph

-- | Prepends a `MachineLabel` to the `Show` output, as a `Text`
labelVertex :: Show a => MachineLabel -> a -> Text
labelVertex label =
  let
    prefix =
      if null (getLabel label)
        then ""
        else getLabel label <> "_"
   in
    (prefix <>) . pack . show

-- | Render all the vertices of a graph after labelling all of them
renderLabelledVertices
  :: forall a
   . (Show a, RenderableVertices a)
  => MachineLabel
  -> Graph a
  -> Mermaid
renderLabelledVertices label _ =
  Mermaid . mconcat . intersperse "\n" $ labelVertex label <$> (vertices :: [a])

-- | Render all vertices with no label
renderVertices :: forall a. (Show a, RenderableVertices a) => Graph a -> Mermaid
renderVertices = renderLabelledVertices ""

-- | Render all the edges of a graph after labelling all of them
renderLabelledEdges :: Show a => MachineLabel -> Graph a -> Mermaid
renderLabelledEdges label (Graph l) =
  Mermaid . mconcat . intersperse "\n" $
    (\(a1, a2) -> labelVertex label a1 <> " --> " <> labelVertex label a2) <$> l

-- | Render all edges with no label
renderEdges :: Show a => Graph a -> Mermaid
renderEdges = renderLabelledEdges ""

-- | Join the outputs of `renderLabelledVertices` and `renderLabelledEdges` to
-- render an entire `Graph`
renderLabelledGraph
  :: (RenderableVertices a, Show a)
  => MachineLabel
  -> Graph a
  -> Mermaid
renderLabelledGraph label graph =
  renderLabelledVertices label graph <> renderLabelledEdges label graph

-- | Render a `Graph` with no labels
renderGraph :: (RenderableVertices a, Show a) => Graph a -> Mermaid
renderGraph = renderLabelledGraph ""

-- | Turn a `Topology` into a `Graph`
topologyAsGraph :: Topology v -> Graph v
topologyAsGraph (Topology edges) = Graph $ edges >>= edgify
  where
    edgify :: (v, [v]) -> [(v, v)]
    edgify (v, vs) = (v,) <$> vs

-- | Interpret a `BaseMachine` as a `Graph` using the information contained in
-- its topology.
--
-- This is the point where we make usage of the machinery provided by the
-- [singletons](https://hackage.haskell.org/package/singletons) library, which
-- require us to impose the constraints we have on @vertex@ and @topology@
baseMachineAsGraph
  :: forall vertex topology input output m
   . ( Demote vertex ~ vertex
     , SingKind vertex
     , SingI topology
     )
  => BaseMachineT m (topology :: Topology vertex) input output
  -> Graph vertex
baseMachineAsGraph _ = topologyAsGraph (demote @topology)

-- | Render an `UntypedGraph` to the Mermaid format
renderUntypedStateDiagram :: UntypedGraph -> Mermaid
renderUntypedStateDiagram (UntypedGraph graph) = renderStateDiagram graph

-- | Render an `UntypedGraph`
renderUntypedGraph :: UntypedGraph -> Mermaid
renderUntypedGraph (UntypedGraph graph) = renderGraph graph

-- | Interpret a `StateMachine` as an `UntypedGraph` using the information
-- contained in its structure and in the topology of its basic components
machineAsGraph :: StateMachineT m input output -> UntypedGraph
machineAsGraph (Basic baseMachine) =
  UntypedGraph (baseMachineAsGraph baseMachine)
machineAsGraph (Sequential machine1 machine2) =
  untypedRemoveIdentityEdges $
    untypedProductGraph
      (untypedAddIdentityEdges $ machineAsGraph machine1)
      (untypedAddIdentityEdges $ machineAsGraph machine2)
machineAsGraph (Parallel machine1 machine2) =
  untypedRemoveIdentityEdges $
    untypedProductGraph
      (untypedAddIdentityEdges $ machineAsGraph machine1)
      (untypedAddIdentityEdges $ machineAsGraph machine2)
machineAsGraph (Alternative machine1 machine2) =
  untypedRemoveIdentityEdges $
    untypedProductGraph
      (untypedAddIdentityEdges $ machineAsGraph machine1)
      (untypedAddIdentityEdges $ machineAsGraph machine2)
machineAsGraph (Feedback machine1 machine2) =
  untypedRemoveIdentityEdges $
    untypedTransitiveClosureGraph $
      untypedProductGraph
        (untypedAddIdentityEdges $ machineAsGraph machine1)
        (untypedAddIdentityEdges $ machineAsGraph machine2)
machineAsGraph (Kleisli machine1 machine2) =
  untypedRemoveIdentityEdges $
    untypedProductGraph
      (untypedAddIdentityEdges $ machineAsGraph machine1)
      (untypedAddIdentityEdges $ untypedTransitiveClosureGraph $ machineAsGraph machine2)
