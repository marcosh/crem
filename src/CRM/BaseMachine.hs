{-# LANGUAGE DataKinds #-}

module CRM.BaseMachine where

import CRM.Topology
import "base" Data.Bifunctor (Bifunctor (..), first)
import "base" Data.Kind (Type)

-- import "base" Data.List.NonEmpty (NonEmpty (..))
import "profunctors" Data.Profunctor (Choice (..), Profunctor (..), Strong (..))

-- import "profunctors" Data.Profunctor.Sieve (Cosieve (..))

import "base" Data.Functor.Identity (Identity (..))
import "singletons-base" Data.Singletons.Base.TH (STuple0 (..))

-- * Specifying state machines

-- | A `BaseMachineT topology input output` describes a state machine with
-- allowed transitions constrained by a given `topology`.
-- A state machine is composed by an `initialState` and an `action`, which
-- defines the `output` and the new `state` given the current `state` and an
-- `input`
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

type BaseMachine
  (topology :: Topology vertex)
  (input :: Type)
  (output :: Type) =
  forall m. Monad m => BaseMachineT m topology input output

-- * Hoist

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

-- -- | see https://hackage.haskell.org/package/profunctors-5.6.2/docs/Data-Profunctor-Sieve.html#v:cosieve
-- -- This is basically saying that we can interpret a `BaseMachineT m topology a b`
-- -- as a function from a `NonEmpty a` to `b`
-- instance Cosieve (BaseMachineT m topology) NonEmpty where
--   cosieve :: BaseMachineT m topology a b -> NonEmpty a -> m b
--   cosieve machine (a0 :| as0) =
--     case runBaseMachineT machine a0 of
--       (b, machine') -> case as0 of
--         [] -> b
--         a1 : as1 -> cosieve machine' (a1 :| as1)

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

pureResult
  :: (Applicative m, AllowedTransition topology initialVertex finalVertex)
  => output
  -> state finalVertex
  -> ActionResult m topology state initialVertex output
pureResult output state = ActionResult . pure $ (output, state)

sequence
  :: ActionResult Identity topology state initialVertex [output]
  -> ActionResult [] topology state initialVertex output
sequence (ActionResult (Identity (outputs, state))) =
  ActionResult $ (,state) <$> outputs

-- ** Stateless machines

-- | The `statelessBaseT` transforms its input to its output and never changes its state
statelessBaseT :: Applicative m => (a -> m b) -> BaseMachineT m (TrivialTopology @()) a b
statelessBaseT f =
  BaseMachineT
    { initialState = InitialState STuple0
    , action = \state input ->
        ActionResult $ (,state) <$> f input
    }

statelessBase :: (a -> b) -> BaseMachine (TrivialTopology @()) a b
statelessBase f = statelessBaseT (pure . f)

-- ** Identity machine

-- | The `identity` machine simply outputs its input and never changes its state. It is the result of `statelessBase id`.
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

-- | Given an `input`, run the machine to get an output and a new version of
-- the machine
runBaseMachineT
  :: Functor m
  => BaseMachineT m topology input output
  -> input
  -> m (output, BaseMachineT m topology input output)
runBaseMachineT (BaseMachineT (InitialState initialState) action) input =
  let
    actionResult = action initialState input
   in
    case actionResult of
      ActionResult outputStatePair ->
        second
          ( \finalState ->
              BaseMachineT
                { initialState = InitialState finalState
                , action = action
                }
          )
          <$> outputStatePair
