{-# LANGUAGE DataKinds #-}

module CRM.BaseMachine where

import CRM.Topology
import "base" Data.Kind (Type)
import "profunctors" Data.Profunctor (Choice (..), Profunctor (..), Strong (..))
import "singletons-base" Data.Singletons.Base.TH (STuple0 (..))

-- * Specifying state machines

-- | A `BaseMachine topology input output` describes a state machine with
-- allowed transitions constrained by a given `topology`.
-- A state machine is composed by an `initialState` and an `action`, which
-- defines the `output` and the new `state` given the current `state` and an
-- `input`
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

instance Profunctor (BaseMachine topology) where
  lmap :: (a -> b) -> BaseMachine topology b c -> BaseMachine topology a c
  lmap f (BaseMachine initialState action) =
    BaseMachine
      { initialState = initialState
      , action = (. f) . action
      }

  rmap :: (b -> c) -> BaseMachine topology a b -> BaseMachine topology a c
  rmap f (BaseMachine initialState action) =
    BaseMachine
      { initialState = initialState
      , action = ((f <$>) .) . action
      }

instance Strong (BaseMachine topology) where
  first' :: BaseMachine topology a b -> BaseMachine topology (a, c) (b, c)
  first' (BaseMachine initialState action) =
    BaseMachine
      { initialState = initialState
      , action = \state (a, c) -> (,c) <$> action state a
      }

  second' :: BaseMachine topology a b -> BaseMachine topology (c, a) (c, b)
  second' (BaseMachine initialState action) =
    BaseMachine
      { initialState = initialState
      , action = \state (c, a) -> (c,) <$> action state a
      }

instance Choice (BaseMachine topology) where
  left' :: BaseMachine topology a b -> BaseMachine topology (Either a c) (Either b c)
  left' (BaseMachine initialState action) =
    BaseMachine
      { initialState = initialState
      , action = \state -> \case
          Left a -> Left <$> action state a
          Right c -> ActionResult state (Right c)
      }

  right' :: BaseMachine topology a b -> BaseMachine topology (Either c a) (Either c b)
  right' (BaseMachine initialState action) =
    BaseMachine
      { initialState = initialState
      , action = \state -> \case
          Left c -> ActionResult state (Left c)
          Right a -> Right <$> action state a
      }

-- | A value of type `InitialState state` describes the initial state of a
-- state machine, describing the initial `vertex` in the `topology` and the
-- actual initial data of type `state vertex`
data InitialState (state :: vertex -> Type) where
  InitialState :: state vertex -> InitialState state

-- | The result of an action of the state machine.
-- An `ActionResult topology state initialVertex output` contains an `output` and a `state finalVertex`,
-- where the transition from `initialVertex` to `finalVertex` is allowed by the machine `topology`
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

instance Functor (ActionResult topology state initialVertex) where
  fmap
    :: (a -> b)
    -> ActionResult topology state initialVertex a
    -> ActionResult topology state initialVertex b
  fmap f (ActionResult state output) =
    ActionResult state (f output)

-- ** Stateless machines

-- | The `statelessBase` transforms its input to its output and never changes its state
statelessBase :: (a -> b) -> BaseMachine TrivialTopology a b
statelessBase f =
  BaseMachine
    { initialState = InitialState STuple0
    , action = \state input ->
        ActionResult state $ f input
    }

-- ** Identity machine

-- | The `identity` machine simply outputs its input and never changes its state. It is the result of `statelessBase id`.
identity :: BaseMachine TrivialTopology a a
identity = statelessBase id

-- ** Unfold

-- | a machine modelled with explicit state, where every transition is allowed
unrestrictedBaseMachine
  :: (forall initialVertex. state initialVertex -> a -> ActionResult (AllowAllTopology @vertex) state initialVertex b)
  -> InitialState (state :: vertex -> Type)
  -> BaseMachine (AllowAllTopology @vertex) a b
unrestrictedBaseMachine action initialState =
  BaseMachine
    { initialState = initialState
    , action = action
    }

-- ** Run a machine

-- | Given an `input`, run the machine to get an output and a new version of
-- the machine
runBaseMachine
  :: BaseMachine topology input output
  -> input
  -> (output, BaseMachine topology input output)
runBaseMachine (BaseMachine (InitialState initialState) action) input =
  let
    actionResult = action initialState input
   in
    case actionResult of
      (ActionResult finalState output) ->
        ( output
        , BaseMachine
            { initialState = InitialState finalState
            , action = action
            }
        )
