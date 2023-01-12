{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag--Wredundant-constraints
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
-- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag--Wunused-type-patterns
{-# OPTIONS_GHC -Wno-unused-type-patterns #-}

module CRM.Topology where

import "singletons-base" Data.Singletons.Base.TH (singletons)

-- * Topology

-- | A `Topology` is a description of the topology of a state machine
-- It contains the collection of allowed transitions
$( singletons
    [d|
      newtype Topology vertex = Topology {edges :: [(vertex, [vertex])]}
      |]
 )

-- ** AllowedTransition

-- | We expose at the type level the information contained in the topology
-- An instance of `AllowedTransition topology initial final` means that the
-- `topology` allows transitions from the`initial` to the `final` state
class AllowedTransition (topology :: Topology vertex) (initial :: vertex) (final :: vertex)

-- | If `a` is the start and `b` is the end of the first edge,
-- then `map` contains an edge from `a` to `b`
instance {-# OVERLAPPING #-} AllowedTransition ('Topology ('(a, b : l1) : l2)) a b

-- | If we know that we have an edge from `a` to `b` in `map`,
-- then we also have an edge from `a` to `b` if we add another edge out of `a`
instance {-# OVERLAPPING #-} AllowedTransition ('Topology ('(a, l1) : l2)) a b => AllowedTransition ('Topology ('(a, x : l1) : l2)) a b

-- | If we know that we have an edge from `a` to `b` in `map`,
-- then we also have an edge from `a` to `b` if we add another vertex
instance AllowedTransition ('Topology map) a b => AllowedTransition ('Topology (x : map)) a b
