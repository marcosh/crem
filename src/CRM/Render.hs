{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CRM.Render where

import CRM.BaseMachine
import CRM.Graph
import CRM.StateMachine
import CRM.Topology
import "singletons-base" Data.Singletons (Demote, SingI, SingKind, demote)
import "text" Data.Text (Text, pack)

newtype Mermaid = Mermaid {getText :: Text}
  deriving newtype (Eq, Show)

instance Semigroup Mermaid where
  (<>) :: Mermaid -> Mermaid -> Mermaid
  (Mermaid t1) <> (Mermaid t2) = Mermaid (t1 <> "\n" <> t2)

-- | We can render a `Graph a` as [mermaid](https://mermaid.js.org/) state diagram
renderStateDiagram :: Show a => Graph a -> Mermaid
renderStateDiagram graph =
  Mermaid "stateDiagram-v2\n" <> renderGraph graph

renderGraph :: Show a => Graph a -> Mermaid
renderGraph (Graph l) =
  Mermaid $
    foldMap (\(a1, a2) -> pack (show a1) <> " --> " <> pack (show a2) <> "\n") l

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
   . (Demote vertex ~ vertex, SingKind vertex, SingI topology)
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
