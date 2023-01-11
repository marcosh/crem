module CRM.Graph where

-- * Graph

-- | A graph is just a list of edges between vertices of type `a`
newtype Graph a = Graph [(a, a)]
  deriving stock (Eq, Show)

{- | The product graph.
 It has as vertices the product of the set of vertices of the initial graph.
 It has as edge from `(a1, b1)` to `(a2, b2)` if and only if there is an edge
 from `a1` to `a2` and an edge from `b1` to `b2`

 >>> productGraph (Graph [('a', 'b')]) (Graph [('c', 'd')])
 Graph [(('a','c'),('b','d'))]
-}
productGraph :: Graph a -> Graph b -> Graph (a, b)
productGraph (Graph edges1) (Graph edges2) =
  Graph $
    ( \((initialEdge1, finalEdge1), (initialEdge2, finalEdge2)) ->
        ((initialEdge1, initialEdge2), (finalEdge1, finalEdge2))
    )
      <$> [(edge1, edge2) | edge1 <- edges1, edge2 <- edges2]

-- * UntypedGraph

-- A data type to represent a graph which is not tracking the vertex type
data UntypedGraph = forall a. (Show a) => UntypedGraph (Graph a)

instance Show UntypedGraph where
  show :: UntypedGraph -> String
  show (UntypedGraph graph) = show graph

-- same as `productGraph` but for `UntypedGraph`
untypedProductGraph :: UntypedGraph -> UntypedGraph -> UntypedGraph
untypedProductGraph (UntypedGraph graph1) (UntypedGraph graph2) =
  UntypedGraph (productGraph graph1 graph2)
