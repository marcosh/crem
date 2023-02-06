module Crem.Graph where

import Crem.Render.RenderableVertices (RenderableVertices)
import "base" Data.List (nub)

-- * Graph

-- | A graph is just a list of edges between vertices of type `a`
newtype Graph a = Graph [(a, a)]
  deriving stock (Eq, Show)

-- | The product graph.
-- It has as vertices the product of the set of vertices of the initial graph.
-- It has as edge from `(a1, b1)` to `(a2, b2)` if and only if there is an edge
-- from `a1` to `a2` and an edge from `b1` to `b2`
--
-- >>> productGraph (Graph [('a', 'b')]) (Graph [('c', 'd')])
-- Graph [(('a','c'),('b','d'))]
productGraph :: Graph a -> Graph b -> Graph (a, b)
productGraph (Graph edges1) (Graph edges2) =
  Graph $
    ( \((initialEdge1, finalEdge1), (initialEdge2, finalEdge2)) ->
        ((initialEdge1, initialEdge2), (finalEdge1, finalEdge2))
    )
      <$> [(edge1, edge2) | edge1 <- edges1, edge2 <- edges2]

-- computes all the possible paths in the input graph and considers them as
-- edges.
-- Notive that the current implementation is removing duplicates
transitiveClosureGraph :: Eq a => Graph a -> Graph a
transitiveClosureGraph graph@(Graph edges) =
  Graph $
    foldr
      ( \a edgesSoFar ->
          edgesSoFar <> pathsFrom graph a
      )
      []
      (nub $ fst <$> edges)
  where
    edgesFrom :: Eq a => Graph a -> a -> [(a, a)]
    edgesFrom (Graph edges') a = filter ((== a) . fst) edges'

    pathsFrom :: forall a. Eq a => Graph a -> a -> [(a, a)]
    pathsFrom g a =
      let
        edgesFromAToB = edgesFrom g a
        pathsFromBToC = edgesFromAToB >>= pathsFrom g . snd
        edgesFromAToC = (a,) . snd <$> pathsFromBToC
       in
        edgesFromAToB <> edgesFromAToC

-- * UntypedGraph

-- A data type to represent a graph which is not tracking the vertex type
data UntypedGraph = forall a. (RenderableVertices a, Eq a, Show a) => UntypedGraph (Graph a)

instance Show UntypedGraph where
  show :: UntypedGraph -> String
  show (UntypedGraph graph) = show graph

-- same as `productGraph` but for `UntypedGraph`
untypedProductGraph :: UntypedGraph -> UntypedGraph -> UntypedGraph
untypedProductGraph (UntypedGraph graph1) (UntypedGraph graph2) =
  UntypedGraph (productGraph graph1 graph2)

-- same as `transitiveClosureGraph` but for `UntypedGraph`
untypedTransitiveClosureGraph :: UntypedGraph -> UntypedGraph
untypedTransitiveClosureGraph (UntypedGraph graph) =
  UntypedGraph (transitiveClosureGraph graph)
