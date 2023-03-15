{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

-- | This is the main module of the whole library. It defines the central
-- `StateMachineT` type, which allows us to create composable state machines.
module Crem.StateMachine where

import Crem.BaseMachine as BaseMachine
import Crem.Render.RenderableVertices (RenderableVertices)
import Crem.Topology (AllowAllTopology, Topology)
import "base" Control.Arrow (Arrow (arr, first), ArrowChoice (left))
import "base" Control.Category (Category (..))
import "base" Data.Bifunctor (Bifunctor (second), bimap)
import "base" Data.Foldable (foldlM)
import "base" Data.Kind (Type)
import "profunctors" Data.Profunctor (Choice (..), Profunctor (..), Strong (..))
import "singletons-base" Data.Singletons (Demote, SingI, SingKind)
import Prelude hiding ((.))

-- | A `StateMachineT` is an effectful [Mealy machine](https://en.wikipedia.org/wiki/Mealy_machine)
-- with inputs of type @input@ and outputs of type @output@
--
-- Effects are described by the context @m@ in which the action of the machine
-- is executed
--
-- `StateMachineT` is a tree, where leaves are `BaseMachineT` and other nodes
-- describe how to combine the subtrees to obtain more complex machines.
data StateMachineT m input output where
  Basic
    :: forall m vertex (topology :: Topology vertex) input output
     . ( Demote vertex ~ vertex
       , SingKind vertex
       , SingI topology
       , Eq vertex
       , Show vertex
       , RenderableVertices vertex
       )
    => BaseMachineT m topology input output
    -> StateMachineT m input output
  Sequential
    :: StateMachineT m a b
    -> StateMachineT m b c
    -> StateMachineT m a c
  Parallel
    :: StateMachineT m a b
    -> StateMachineT m c d
    -> StateMachineT m (a, c) (b, d)
  Alternative
    :: StateMachineT m a b
    -> StateMachineT m c d
    -> StateMachineT m (Either a c) (Either b d)
  Feedback
    :: (Foldable n, Monoid (n a), Monoid (n b))
    => StateMachineT m a (n b)
    -> StateMachineT m b (n a)
    -> StateMachineT m a (n b)
  Kleisli
    :: (Foldable n, Monoid (n c))
    => StateMachineT m a (n b)
    -> StateMachineT m b (n c)
    -> StateMachineT m a (n c)

-- | A `StateMachine` is an effectful machine for every possible monad @m@.
-- Needing to work for every monad, in fact it can not perform any kind of
-- effect and needs to be pure in nature.
type StateMachine a b = forall m. Monad m => StateMachineT m a b

-- * Hoist

-- | Allows to change the context @m@ where the machine operates to another
-- context @n@, provided we have a [natural transformation](https://stackoverflow.com/a/58364172/2718064)
-- from @m@ to @n@
hoist :: (forall x. m x -> n x) -> StateMachineT m a b -> StateMachineT n a b
hoist f machine = case machine of
  Basic baseMachine -> Basic $ baseHoist f baseMachine
  Sequential machine1 machine2 -> Sequential (hoist f machine1) (hoist f machine2)
  Parallel machine1 machine2 -> Parallel (hoist f machine1) (hoist f machine2)
  Alternative machine1 machine2 -> Alternative (hoist f machine1) (hoist f machine2)
  Feedback machine1 machine2 -> Feedback (hoist f machine1) (hoist f machine2)
  Kleisli machine1 machine2 -> Kleisli (hoist f machine1) (hoist f machine2)

-- | a state machine which does not rely on state
statelessT :: Applicative m => (a -> m b) -> StateMachineT m a b
statelessT f = Basic $ statelessBaseT f

-- | a state machine which does not rely on state and does not perform side
-- effects
stateless :: Applicative m => (a -> b) -> StateMachineT m a b
stateless f = statelessT (pure . f)

-- | a machine modelled with explicit state, where every transition is allowed
unrestrictedMachine
  :: ( Demote vertex ~ vertex
     , SingKind vertex
     , SingI (AllowAllTopology @vertex)
     , Eq vertex
     , Show vertex
     , RenderableVertices vertex
     )
  => ( forall initialVertex
        . state initialVertex
       -> a
       -> ActionResult m (AllowAllTopology @vertex) state initialVertex b
     )
  -> InitialState (state :: vertex -> Type)
  -> StateMachineT m a b
unrestrictedMachine action state = Basic $ unrestrictedBaseMachineT action state

-- * Category

instance Monad m => Category (StateMachineT m) where
  id :: StateMachineT m a a
  id = Basic identity

  (.) :: StateMachineT m b c -> StateMachineT m a b -> StateMachineT m a c
  (.) = flip Sequential

-- * Profunctor

instance Applicative m => Profunctor (StateMachineT m) where
  lmap :: (a -> b) -> StateMachineT m b c -> StateMachineT m a c
  lmap f (Basic baseMachine) = Basic $ lmap f baseMachine
  lmap f (Sequential machine1 machine2) = Sequential (lmap f machine1) machine2
  lmap f machine = Sequential (stateless f) machine

  rmap :: (b -> c) -> StateMachineT m a b -> StateMachineT m a c
  rmap f (Basic baseMachine) = Basic $ rmap f baseMachine
  rmap f (Sequential machine1 machine2) = Sequential machine1 (rmap f machine2)
  rmap f machine = Sequential machine (stateless f)

-- * Strong

instance Monad m => Strong (StateMachineT m) where
  first' :: StateMachineT m a b -> StateMachineT m (a, c) (b, c)
  first' = flip Parallel Control.Category.id

  second' :: StateMachineT m a b -> StateMachineT m (c, a) (c, b)
  second' = Parallel Control.Category.id

-- * Choice

-- | An instance of `Choice` allows us to have parallel composition of state
-- machines, meaning that we can pass two inputs to two state machines and get
-- out the outputs of both
instance Monad m => Choice (StateMachineT m) where
  left' :: StateMachineT m a b -> StateMachineT m (Either a c) (Either b c)
  left' = flip Alternative Control.Category.id

  right' :: StateMachineT m a b -> StateMachineT m (Either c a) (Either c b)
  right' = Alternative Control.Category.id

-- * Arrow

instance Monad m => Arrow (StateMachineT m) where
  arr :: (a -> b) -> StateMachineT m a b
  arr = stateless

  first :: StateMachineT m a b -> StateMachineT m (a, c) (b, c)
  first = first'

-- * ArrowChoice

instance Monad m => ArrowChoice (StateMachineT m) where
  left :: StateMachineT m a b -> StateMachineT m (Either a c) (Either b c)
  left = left'

-- * Run a state machine

-- | Given an @input@, run the machine to get an output and a new version of
-- the machine
run :: Monad m => StateMachineT m a b -> a -> m (b, StateMachineT m a b)
run (Basic baseMachine) a = second Basic <$> runBaseMachineT baseMachine a
run (Sequential machine1 machine2) a = do
  (output1, machine1') <- run machine1 a
  (output2, machine2') <- run machine2 output1
  pure (output2, Sequential machine1' machine2')
run (Parallel machine1 machine2) a = do
  (output1, machine1') <- run machine1 (fst a)
  (output2, machine2') <- run machine2 (snd a)
  pure ((output1, output2), Parallel machine1' machine2')
run (Alternative machine1 machine2) a =
  case a of
    Left a1 -> bimap Left (`Alternative` machine2) <$> run machine1 a1
    Right a2 -> bimap Right (machine1 `Alternative`) <$> run machine2 a2
run (Feedback machine1 machine2) a = do
  (bs, machine1') <- run machine1 a
  (as, machine2') <- runMultiple machine2 bs
  first (bs <>) <$> runMultiple (Feedback machine1' machine2') as
run (Kleisli machine1 machine2) a = do
  (bs, machine1') <- run machine1 a
  (cs, machine2') <- runMultiple machine2 bs
  pure (cs, Kleisli machine1' machine2')

-- | process multiple inputs in one go, accumulating the results in a monoid
runMultiple
  :: (Monad m, Foldable f, Monoid b)
  => StateMachineT m a b
  -> f a
  -> m (b, StateMachineT m a b)
runMultiple machine =
  foldlM
    (\(b, machine') a -> first (b <>) <$> run machine' a)
    (mempty, machine)
