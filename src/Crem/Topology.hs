{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag--Wredundant-constraints
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
-- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag--Wunused-type-patterns
{-# OPTIONS_GHC -Wno-unused-type-patterns #-}

-- | A `Topology` is a list of allowed transition for a state machine.
-- We are using it to enforce that only allowed transitions could be performed.
module Crem.Topology
  ( Topology (..)
  , TopologySym0
  , STopology (..)
  , AllowTransition (..)
  , AllowedTransition (..)
  , trivialTopology
  , sTrivialTopology
  , TrivialTopology
  , allowAllTopology
  , sAllowAllTopology
  , AllowAllTopology
  )
where

import "singletons-base" Data.Singletons.Base.TH
import "singletons-base" Prelude.Singletons

-- * Topology

-- | A `Topology` is a description of the topology of a state machine
-- It contains the collection of allowed transitions.
-- Since we are using this information at the type level, and then we want to
-- bring it down to the value level, we wrap it in `singletons`
$( singletons
    [d|
      newtype Topology vertex = Topology
        {edges :: [(vertex, [vertex])]}
      |]
 )

-- ** AllowedTransition

-- | An value of type of @AllowedTransition topology initial final@ is a proof
-- that the @topology@ allows transitions from the @initial@ to the @final@
-- state
data AllowTransition (topology :: Topology vertex) (initial :: vertex) (final :: vertex) where
  -- | We always allow an edge from a vertex to itself
  AllowIdentityEdge :: AllowTransition topology a a
  -- | If @a@ is the start and @b@ is the end of the first edge,
  -- then @map@ contains an edge from @a@ to @b@
  AllowFirstEdge :: AllowTransition ('Topology ('(a, b : l1) : l2)) a b
  -- | If we know that we have an edge from @a@ to @b@ in a topology,
  -- then we also have an edge from @a@ to @b@ if we add another edge out of @a@
  AllowAddingEdge
    :: AllowTransition ('Topology ('(a, l1) : l2)) a b
    -> AllowTransition ('Topology ('(a, x : l1) : l2)) a b
  -- | If we know that we have an edge from @a@ to @b@ in @map@,
  -- then we also have an edge from @a@ to @b@ if we add another vertex
  AllowAddingVertex
    :: AllowTransition ('Topology map) a b
    -> AllowTransition ('Topology (x : map)) a b

-- | The `AllowedTransition` type class enables to automatically perform proof search
-- for a `AllowTransition` term.
-- It has an instance for every constructor of `AllowTransition`
class AllowedTransition (topology :: Topology vertex) (initial :: vertex) (final :: vertex) where
  allowsTransition :: AllowTransition topology initial final

instance {-# INCOHERENT #-} AllowedTransition ('Topology ('(a, b : l1) : l2)) a b where
  allowsTransition = AllowFirstEdge

instance {-# INCOHERENT #-} AllowedTransition ('Topology ('(a, l1) : l2)) a b => AllowedTransition ('Topology ('(a, x : l1) : l2)) a b where
  allowsTransition =
    AllowAddingEdge (allowsTransition :: AllowTransition ('Topology ('(a, l1) : l2)) a b)

instance {-# INCOHERENT #-} AllowedTransition ('Topology map) a b => AllowedTransition ('Topology (x : map)) a b where
  allowsTransition =
    AllowAddingVertex (allowsTransition :: AllowTransition ('Topology map) a b)

instance {-# INCOHERENT #-} AllowedTransition topology a a where
  allowsTransition = AllowIdentityEdge

-- ** Trivial topology

-- | The trivial topology only allows identity transitions.
-- Given a type @a@ for vertices, only trivial transitions, i.e. staying
-- at the same vertex, are allowed
$( singletons
    [d|
      trivialTopology :: Topology a
      trivialTopology = Topology []
      |]
 )

-- ** Allow all topology

-- | Given a type @a@ for vertices, every transition from one vertex to
-- any other is allowed
$( singletons
    [d|
      allowAllTopology :: (Bounded a, Enum a) => Topology a
      allowAllTopology = Topology [(a, [minBound .. maxBound]) | a <- [minBound .. maxBound]]
      |]
 )
