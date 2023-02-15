{-# LANGUAGE DataKinds #-}

-- | A `BaseMachine` is a Mealy machine constrained by a provided `Topology` of
-- allowed transitions.
module Crem.BaseMachine where

import Crem.Topology
import "base" Data.Bifunctor (Bifunctor (..), first)
import "base" Data.Functor.Identity (Identity (..))
import "base" Data.Kind (Type)
import "profunctors" Data.Profunctor (Choice (..), Profunctor (..), Strong (..))
import "singletons-base" Data.Singletons.Base.TH (STuple0 (..))

-- * Specifying state machines

-- | A @BaseMachineT m topology input output@ describes a state machine with
-- allowed transitions constrained by a given @topology@.
-- A state machine is composed by an `initialState` and an `action`, which
-- defines the @output@ and the new @state@ given the current @state@ and an
-- @input@.
-- The @m@ parameter describes the context where the `action` of the machine is
-- executed
data
  BaseMachineT
    m
    (topology :: Topology vertex)
    (input :: Type)
    (output :: Type) = forall state.
  BaseMachineT
  { initialState :: InitialState state
  , action
      :: forall initialVertex
       . state initialVertex
      -> input
      -> ActionResult m topology state initialVertex output
  }

-- | A `BaseMachine` is an effectful machine for every possible monad @m@.
-- Needing to work for every monad, in fact it can not perform any kind of
-- effect and needs to be pure in nature.
type BaseMachine
  (topology :: Topology vertex)
  (input :: Type)
  (output :: Type) =
  forall m. Monad m => BaseMachineT m topology input output

-- * Hoist

-- | Allows to change the context @m@ where the machine operates to another
-- context @n@, provided we have a [natural transformation](https://stackoverflow.com/a/58364172/2718064)
-- from @m@ to @n@
baseHoist
  :: (forall x. m x -> n x)
  -> BaseMachineT m topology a b
  -> BaseMachineT n topology a b
baseHoist f (BaseMachineT initialState action) =
  BaseMachineT
    initialState
    ((hoistResult f .) . action)

instance Functor m => Profunctor (BaseMachineT m topology) where
  lmap :: (a -> b) -> BaseMachineT m topology b c -> BaseMachineT m topology a c
  lmap f (BaseMachineT initialState action) =
    BaseMachineT
      { initialState = initialState
      , action = (. f) . action
      }

  rmap :: (b -> c) -> BaseMachineT m topology a b -> BaseMachineT m topology a c
  rmap f (BaseMachineT initialState action) =
    BaseMachineT
      { initialState = initialState
      , action = ((f <$>) .) . action
      }

instance Functor m => Strong (BaseMachineT m topology) where
  first' :: BaseMachineT m topology a b -> BaseMachineT m topology (a, c) (b, c)
  first' (BaseMachineT initialState action) =
    BaseMachineT
      { initialState = initialState
      , action = \state (a, c) -> (,c) <$> action state a
      }

  second' :: BaseMachineT m topology a b -> BaseMachineT m topology (c, a) (c, b)
  second' (BaseMachineT initialState action) =
    BaseMachineT
      { initialState = initialState
      , action = \state (c, a) -> (c,) <$> action state a
      }

instance Applicative m => Choice (BaseMachineT m topology) where
  left' :: BaseMachineT m topology a b -> BaseMachineT m topology (Either a c) (Either b c)
  left' (BaseMachineT initialState action) =
    BaseMachineT
      { initialState = initialState
      , action = \state -> \case
          Left a -> Left <$> action state a
          Right c -> ActionResult $ pure (Right c, state)
      }

  right' :: BaseMachineT m topology a b -> BaseMachineT m topology (Either c a) (Either c b)
  right' (BaseMachineT initialState action) =
    BaseMachineT
      { initialState = initialState
      , action = \state -> \case
          Left c -> ActionResult $ pure (Left c, state)
          Right a -> Right <$> action state a
      }

-- | A value of type @InitialState state@ describes the initial state of a
-- state machine, describing the initial @vertex@ in the @topology@ and the
-- actual initial data of type @state vertex@
data InitialState (state :: vertex -> Type) where
  InitialState :: state vertex -> InitialState state

-- | The result of an action of the state machine.
-- An @ActionResult m topology state initialVertex output@ contains an @output@
-- and a @state finalVertex@, where the transition from @initialVertex@ to
-- @finalVertex@ is allowed by the machine @topology@
data
  ActionResult
    m
    (topology :: Topology vertex)
    (state :: vertex -> Type)
    (initialVertex :: vertex)
    (output :: Type)
  where
  ActionResult
    :: AllowedTransition topology initialVertex finalVertex
    => m (output, state finalVertex)
    -> ActionResult m topology state initialVertex output

-- | Allows to change the computational context of an `ActionResult` from @m@
-- to @n@, given we have a [natural transformation](https://stackoverflow.com/a/58364172/2718064)
-- from @m@ to @n@.
hoistResult
  :: (forall x. m x -> n x)
  -> ActionResult m topology state initialVertex output
  -> ActionResult n topology state initialVertex output
hoistResult f (ActionResult outputStatePair) = ActionResult $ f outputStatePair

instance Functor m => Functor (ActionResult m topology state initialVertex) where
  fmap
    :: (a -> b)
    -> ActionResult m topology state initialVertex a
    -> ActionResult m topology state initialVertex b
  fmap f (ActionResult outputStatePair) =
    ActionResult $ first f <$> outputStatePair

-- | Create an `ActionResult` without performing any side effect in the @m@
-- context
pureResult
  :: (Applicative m, AllowedTransition topology initialVertex finalVertex)
  => output
  -> state finalVertex
  -> ActionResult m topology state initialVertex output
pureResult output state = ActionResult . pure $ (output, state)

-- | This is fairly similar to `sequenceA` from `Data.Traversable` and in fact
-- it does the same job, with the slight difference that `sequenceA` would
-- return @f (ActionResult Identity topology state initialVertex output)@
sequence
  :: Functor f
  => ActionResult Identity topology state initialVertex (f output)
  -> ActionResult f topology state initialVertex output
sequence (ActionResult (Identity (outputs, state))) =
  ActionResult $ (,state) <$> outputs

-- ** Stateless machines

-- | `statelessBaseT` transforms its input to its output and never changes its
-- state
statelessBaseT :: Applicative m => (a -> m b) -> BaseMachineT m (TrivialTopology @()) a b
statelessBaseT f =
  BaseMachineT
    { initialState = InitialState STuple0
    , action = \state input ->
        ActionResult $ (,state) <$> f input
    }

-- | `statelessBase` transforms its input to its output and never changes its
-- state, without performing any side effect
statelessBase :: (a -> b) -> BaseMachine (TrivialTopology @()) a b
statelessBase f = statelessBaseT (pure . f)

-- ** Identity machine

-- | The `identity` machine simply outputs its input and never changes its
-- state.
identity :: BaseMachine (TrivialTopology @()) a a
identity = statelessBase id

-- ** Unrestricted machines

-- | a machine modelled with explicit state, where every transition is allowed
unrestrictedBaseMachineT
  :: (forall initialVertex. state initialVertex -> a -> ActionResult m (AllowAllTopology @vertex) state initialVertex b)
  -> InitialState (state :: vertex -> Type)
  -> BaseMachineT m (AllowAllTopology @vertex) a b
unrestrictedBaseMachineT action initialState =
  BaseMachineT
    { initialState = initialState
    , action = action
    }

-- ** Run a machine

-- | Given an @input@, run the machine to get an output and a new version of
-- the machine
runBaseMachineT
  :: Functor m
  => BaseMachineT m topology input output
  -> input
  -> m (output, BaseMachineT m topology input output)
runBaseMachineT (BaseMachineT (InitialState initialState) action) input =
  case action initialState input of
    ActionResult outputStatePair ->
      second
        ( \finalState ->
            BaseMachineT
              { initialState = InitialState finalState
              , action = action
              }
        )
        <$> outputStatePair
