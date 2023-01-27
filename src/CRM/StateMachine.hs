{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module CRM.StateMachine where

import CRM.BaseMachine as BaseMachine
import CRM.Topology
import "base" Control.Category (Category (..))
import "base" Data.Bifunctor (Bifunctor (..), bimap)
import "base" Data.Foldable (foldlM)
import "base" Data.Kind (Type)
import "profunctors" Data.Profunctor (Choice {-Costrong (..),-} (..), Profunctor (..), Strong (..))
import "singletons-base" Data.Singletons (Demote, SingI, SingKind)
import Prelude hiding ((.))

-- | A `StateMachine` is a [Mealy machine](https://en.wikipedia.org/wiki/Mealy_machine)
-- with inputs of type `input` and outputs of type `output`
data StateMachineT m input output where
  Basic
    :: forall m vertex (topology :: Topology vertex) input output
     . ( Demote vertex ~ vertex
       , SingKind vertex
       , SingI topology
       , Eq vertex
       , Show vertex
       )
    => BaseMachineT m topology input output
    -> StateMachineT m input output
  Compose
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
  Loop
    :: StateMachineT m a [a]
    -> StateMachineT m a [a]
  Kleisli
    :: (Foldable n, Monoid (n c))
    => StateMachineT m a (n b)
    -> StateMachineT m b (n c)
    -> StateMachineT m a (n c)

type StateMachine a b = forall m. Monad m => StateMachineT m a b

-- * Hoist

hoist :: (forall x. m x -> n x) -> StateMachineT m a b -> StateMachineT n a b
hoist f machine = case machine of
  Basic baseMachine -> Basic $ baseHoist f baseMachine
  Compose machine1 machine2 -> Compose (hoist f machine1) (hoist f machine2)
  Parallel machine1 machine2 -> Parallel (hoist f machine1) (hoist f machine2)
  Alternative machine1 machine2 -> Alternative (hoist f machine1) (hoist f machine2)
  Loop machine' -> Loop $ hoist f machine'
  Kleisli machine1 machine2 -> Kleisli (hoist f machine1) (hoist f machine2)

-- | a state machine which does not rely on state
statelessT :: Applicative m => (a -> m b) -> StateMachineT m a b
statelessT f = Basic $ statelessBaseT f

stateless :: Applicative m => (a -> b) -> StateMachineT m a b
stateless f = statelessT (pure . f)

-- | a machine modelled with explicit state, where every transition is allowed
unrestrictedMachine
  :: ( Demote vertex ~ vertex
     , SingKind vertex
     , SingI (AllowAllTopology @vertex)
     , Eq vertex
     , Show vertex
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
  (.) = flip Compose

-- * Profunctor

instance Applicative m => Profunctor (StateMachineT m) where
  lmap :: (a -> b) -> StateMachineT m b c -> StateMachineT m a c
  lmap f (Basic baseMachine) = Basic $ lmap f baseMachine
  lmap f (Compose machine1 machine2) = Compose (lmap f machine1) machine2
  lmap f machine = Compose (stateless f) machine

  rmap :: (b -> c) -> StateMachineT m a b -> StateMachineT m a c
  rmap f (Basic baseMachine) = Basic $ rmap f baseMachine
  rmap f (Compose machine1 machine2) = Compose machine1 (rmap f machine2)
  rmap f machine = Compose machine (stateless f)

-- * Strong

instance Monad m => Strong (StateMachineT m) where
  first' :: StateMachineT m a b -> StateMachineT m (a, c) (b, c)
  first' = flip Parallel Control.Category.id

  second' :: StateMachineT m a b -> StateMachineT m (c, a) (c, b)
  second' = Parallel Control.Category.id

-- * Choice

-- | An instance of `Choice` allows us to have parallel composition of state machines, meaning that we can pass two inputs to two state machines and get out the outputs of both
instance Monad m => Choice (StateMachineT m) where
  left' :: StateMachineT m a b -> StateMachineT m (Either a c) (Either b c)
  left' = flip Alternative Control.Category.id

  right' :: StateMachineT m a b -> StateMachineT m (Either c a) (Either c b)
  right' = Alternative Control.Category.id

-- -- * Cosieve

-- -- | see https://hackage.haskell.org/package/profunctors-5.6.2/docs/Data-Profunctor-Sieve.html#v:cosieve
-- -- This is basically saying that we can interpret a `StateMachine a b` as a
-- -- function from a `NonEmpty a` to `b`
-- instance Cosieve (StateMachineT m) NonEmpty where
--   cosieve :: StateMachineT m a b -> NonEmpty a -> m b
--   cosieve machine (a0 :| as0) =
--     case run machine a0 of
--       (b, machine') -> case as0 of
--         [] -> b
--         a1 : as1 -> cosieve machine' (a1 :| as1)

-- -- * Corepresentable

-- -- the state space for a machine with a topology containing a single vertex
-- -- and a type of possible states in that vertex
-- data SingleVertexState a (vertex :: ()) where
--   SingleVertexState :: a -> SingleVertexState a '()

-- -- | see https://hackage.haskell.org/package/profunctors-5.6.2/docs/Data-Profunctor-Rep.html#t:Corepresentable
-- -- This is basically saying that we can interpret a function from `NonEmpty a`
-- -- to `b` as a `StateMachine a b`, where we store the tail of the non-empty
-- -- list in the state of the machine.
-- instance Corepresentable (StateMachineT m) where
--   type Corep (StateMachineT m) = NonEmpty

--   cotabulate :: forall a b. (NonEmpty a -> m b) -> StateMachineT m a b
--   cotabulate f =
--     Basic @_ @() @TrivialTopology $
--       BaseMachineT
--         { initialState = InitialState $ SingleVertexState ([] :: [a])
--         , action = \(SingleVertexState as) input ->
--             let
--               allInputs = input : as
--              in
--               ActionResult
--                 (SingleVertexState allInputs)
--                 (f . fromList . reverse $ allInputs)
--         }

-- -- * Costrong

-- instance Costrong (StateMachineT m) where
--   unfirst :: StateMachineT m (a, c) (b, c) -> StateMachineT m a b
--   unfirst = unfirstCorep

--   unsecond :: StateMachineT m (c, a) (c, b) -> StateMachineT m a b
--   unsecond = unsecondCorep

-- * Run a state machine

-- | Given an `input`, run the machine to get an output and a new version of
-- the machine
run :: Monad m => StateMachineT m a b -> a -> m (b, StateMachineT m a b)
run (Basic baseMachine) a = second Basic <$> runBaseMachineT baseMachine a
run (Compose machine1 machine2) a = do
  (output1, machine1') <- run machine1 a
  (output2, machine2') <- run machine2 output1
  pure (output2, Compose machine1' machine2')
run (Parallel machine1 machine2) a = do
  (output1, machine1') <- run machine1 (fst a)
  (output2, machine2') <- run machine2 (snd a)
  pure ((output1, output2), Parallel machine1' machine2')
run (Alternative machine1 machine2) a =
  case a of
    Left a1 -> bimap Left (`Alternative` machine2) <$> run machine1 a1
    Right a2 -> bimap Right (machine1 `Alternative`) <$> run machine2 a2
run (Loop machine) a = do
  (as, machine') <- run machine a
  first (as <>) <$> runMultiple (Loop machine') as
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
