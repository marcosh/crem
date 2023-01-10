{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module CRM.Render where

import CRM.BaseMachine
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
  :: forall tag topology input output
   . (Demote (Topology tag) ~ Topology tag, SingKind tag, SingI topology)
  => BaseMachine (topology :: Topology tag) input output
  -> Graph tag
baseMachineAsGraph _ = topologyAsGraph (demote @topology)
