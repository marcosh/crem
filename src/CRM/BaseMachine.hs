{-# LANGUAGE DataKinds #-}

module CRM.BaseMachine where

import CRM.Topology
import "base" Data.Kind (Type)

-- * Specifying state machines

{- | A `BaseMachine topology input output` describes a state machine with
   allowed transitions constrained by a given `topology`.
   A state machine is composed by an `initialState` and an `action`, which
   defines the `output` and the new `state` given the current `state` and an
   `input`
-}
data
  BaseMachine
    (topology :: Topology vertex)
    (input :: Type)
    (output :: Type) = forall state.
  BaseMachine
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
  InitialState :: state vertex -> InitialState state

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
  ActionResult
    :: AllowedTransition topology initialVertex finalVertex
    => state finalVertex
    -> output
    -> ActionResult topology state initialVertex output
