{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CRM.Render where

import CRM.BaseMachine
import CRM.StateMachine
import CRM.Topology
import "singletons-base" Data.Singletons (Demote, SingI, SingKind, demote)
import "text" Data.Text (Text, pack)

-- * Graph

-- | A graph is just a list of edges between vertices of type `a`
newtype Graph a = Graph [(a, a)]
  deriving stock (Eq, Show)

-- | We can render a `Graph a` as [mermaid](https://mermaid.js.org/) state diagram
renderMermaid :: Show a => Graph a -> Text
renderMermaid (Graph l) =
  "stateDiagram-v2\n"
    <> foldMap (\(a1, a2) -> pack (show a1) <> " --> " <> pack (show a2) <> "\n") l

-- | Turn a `Topology` into a `Graph`
topologyAsGraph :: Topology v -> Graph v
topologyAsGraph (Topology edges) = Graph $ edges >>= edgify
  where
    edgify :: (v, [v]) -> [(v, v)]
    edgify (v, vs) = (v,) <$> vs

{- | Interpret a `BaseMachine` as a `Graph` using the information contained in
  its topology
-}
baseMachineAsGraph
  :: forall vertex topology input output
   . (Demote (Topology vertex) ~ Topology vertex, SingKind vertex, SingI topology)
  => BaseMachine (topology :: Topology vertex) input output
  -> Graph vertex
baseMachineAsGraph _ = topologyAsGraph (demote @topology)

-- A data type to represent a graph which is not tracking the vertex type
data UntypedGraph = forall a. (Show a) => UntypedGraph (Graph a)

instance Show UntypedGraph where
  show :: UntypedGraph -> String
  show (UntypedGraph graph) = show graph

-- Render an `UntypedGraph` to the Mermaid format
renderUntypedMermaid :: UntypedGraph -> Text
renderUntypedMermaid (UntypedGraph graph) = renderMermaid graph

{- | Interpret a `StateMachine` as an `UntypedGraph` using the information
 contained in its structure and in the topology of its basic components
-}
machineAsGraph :: StateMachine input output -> UntypedGraph
machineAsGraph (Basic baseMachine) =
  UntypedGraph (baseMachineAsGraph baseMachine)
