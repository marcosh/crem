{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag--Wredundant-constraints
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
-- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag--Wunused-type-patterns
{-# OPTIONS_GHC -Wno-unused-type-patterns #-}

module CRM.BaseMachine where

import "base" Data.Kind (Type)
import "singletons-base" Data.Singletons.Base.TH (singletons)

-- * Topology

{- | A `Topology` is a description of the topology of a state machine
   It contains the collection of allowed transitions
-}
$( singletons
    [d|
      newtype Topology (vertex :: Type) = MkTopology {edges :: [(vertex, [vertex])]}
      |]
 )

-- ** AllowedTransition

{- | We expose at the type level the information contained in the topology
   An instance of `AllowedTransition topology initial final` means that the
   `topology` allows transitions from the`initial` to the `final` state
-}
class AllowedTransition (topology :: Topology vertex) (initial :: vertex) (final :: vertex)

{- | If `a` is the start and `b` is the end of the first edge,
   then `map` contains an edge from `a` to `b`
-}
instance {-# OVERLAPPING #-} AllowedTransition ('MkTopology ('(a, b : l1) : l2)) a b

{- | If we know that we have an edge from `a` to `b` in `map`,
   then we also have an edge from `a` to `b` if we add another edge out of `a`
-}
instance {-# OVERLAPPING #-} AllowedTransition ('MkTopology ('(a, l1) : l2)) a b => AllowedTransition ('MkTopology ('(a, x : l1) : l2)) a b

{- | If we know that we have an edge from `a` to `b` in `map`,
   then we also have an edge from `a` to `b` if we add another vertex
-}
instance AllowedTransition ('MkTopology map) a b => AllowedTransition ('MkTopology (x : map)) a b

-- * Specifying state machines

{- | A `StateMachine topology input output` describes a state machine with
   allowed transitions constrained by a given `topology`.
   A state machine is composed by an `initialState` and an `action`, which
   defines the `output` and the new `state` given the current `state` and an
   `input`
-}
data
  StateMachine
    (topology :: Topology vertex)
    (input :: Type)
    (output :: Type) = forall state.
  MkStateMachine
  { initialState :: InitialState state
  , action
      :: forall initialVertex
       . state initialVertex
      -> input
      -> ActionResult topology state initialVertex output
  }

{- | A value of type `InitialState state` describes the initial state of a
   state machine, describing the initial `vertex` in the `topology` and the
   actual initial data of type `state vertex`
-}
data InitialState (state :: vertex -> Type) where
  MkInitialState :: state vertex -> InitialState state

{- | The result of an action of the state machine.
   An `ActionResult topology state initialVertex output` contains an `output` and a `state finalVertex`,
   where the transition from `initialVertex` to `finalVertex` is allowed by the machine `topology`
-}
data
  ActionResult
    (topology :: Topology vertex)
    (state :: vertex -> Type)
    (initialVertex :: vertex)
    (output :: Type)
  where
  MkActionResult
    :: AllowedTransition topology initialVertex finalVertex
    => state finalVertex
    -> output
    -> ActionResult topology state initialVertex output
