{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CRM.Render.Render where

import CRM.BaseMachine
import CRM.Graph
import CRM.Render.RenderableVertices
import CRM.StateMachine
import CRM.Topology
import "base" Data.List (intersperse)
import "singletons-base" Data.Singletons (Demote, SingI, SingKind, demote)
import "base" Data.String (IsString)
import "text" Data.Text (Text, null, pack)
import Prelude hiding (null)

newtype Mermaid = Mermaid {getText :: Text}
  deriving newtype (Eq, Show)

instance Semigroup Mermaid where
  (<>) :: Mermaid -> Mermaid -> Mermaid
  (Mermaid "") <> m = m
  m <> (Mermaid "") = m
  (Mermaid t1) <> (Mermaid t2) = Mermaid (t1 <> "\n" <> t2)

newtype MachineLabel = MachineLabel {getLabel :: Text}
  deriving newtype (Eq, Show, IsString)

-- | We can render a `Graph a` as [mermaid](https://mermaid.js.org/) state diagram
renderStateDiagram :: (RenderableVertices a, Show a) => Graph a -> Mermaid
renderStateDiagram graph =
  Mermaid "stateDiagram-v2\n" <> renderGraph graph

labelVertex :: Show a => MachineLabel -> a -> Text
labelVertex label =
  let
    prefix =
      if null (getLabel label)
        then ""
        else getLabel label <> "_"
   in
    (prefix <>) . pack . show

renderLabelledVertices
  :: forall a
   . (Show a, RenderableVertices a)
  => MachineLabel
  -> Graph a
  -> Mermaid
renderLabelledVertices label _ =
  Mermaid . mconcat . intersperse "\n" $ labelVertex label <$> (vertices :: [a])

renderVertices :: forall a. (Show a, RenderableVertices a) => Graph a -> Mermaid
renderVertices = renderLabelledVertices ""

renderLabelledEdges :: Show a => MachineLabel -> Graph a -> Mermaid
renderLabelledEdges label (Graph l) =
  Mermaid . mconcat . intersperse "\n" $
    (\(a1, a2) -> labelVertex label a1 <> " --> " <> labelVertex label a2) <$> l

renderEdges :: Show a => Graph a -> Mermaid
renderEdges = renderLabelledEdges ""

renderLabelledGraph
  :: (RenderableVertices a, Show a)
  => MachineLabel
  -> Graph a
  -> Mermaid
renderLabelledGraph label graph =
  renderLabelledVertices label graph <> renderLabelledEdges label graph

renderGraph :: (RenderableVertices a, Show a) => Graph a -> Mermaid
renderGraph = renderLabelledGraph ""

-- | Turn a `Topology` into a `Graph`
topologyAsGraph :: Topology v -> Graph v
topologyAsGraph (Topology edges) = Graph $ edges >>= edgify
  where
    edgify :: (v, [v]) -> [(v, v)]
    edgify (v, vs) = (v,) <$> vs

-- | Interpret a `BaseMachine` as a `Graph` using the information contained in
-- its topology
baseMachineAsGraph
  :: forall vertex topology input output m
   . ( Demote vertex ~ vertex
     , SingKind vertex
     , SingI topology
     )
  => BaseMachineT m (topology :: Topology vertex) input output
  -> Graph vertex
baseMachineAsGraph _ = topologyAsGraph (demote @topology)

-- Render an `UntypedGraph` to the Mermaid format
renderUntypedStateDiagram :: UntypedGraph -> Mermaid
renderUntypedStateDiagram (UntypedGraph graph) = renderStateDiagram graph

renderUntypedGraph :: UntypedGraph -> Mermaid
renderUntypedGraph (UntypedGraph graph) = renderGraph graph

-- | Interpret a `StateMachine` as an `UntypedGraph` using the information
-- contained in its structure and in the topology of its basic components
machineAsGraph :: StateMachineT m input output -> UntypedGraph
machineAsGraph (Basic baseMachine) =
  UntypedGraph (baseMachineAsGraph baseMachine)
machineAsGraph (Compose machine1 machine2) =
  untypedProductGraph
    (machineAsGraph machine1)
    (machineAsGraph machine2)
machineAsGraph (Parallel machine1 machine2) =
  untypedProductGraph
    (machineAsGraph machine1)
    (machineAsGraph machine2)
machineAsGraph (Alternative machine1 machine2) =
  untypedProductGraph
    (machineAsGraph machine1)
    (machineAsGraph machine2)
machineAsGraph (Feedback machine1 machine2) =
  untypedTransitiveClosureGraph $
    untypedProductGraph
      (machineAsGraph machine1)
      (machineAsGraph machine2)
machineAsGraph (Kleisli machine1 machine2) =
  untypedProductGraph
    (machineAsGraph machine1)
    (untypedTransitiveClosureGraph $ machineAsGraph machine2)
