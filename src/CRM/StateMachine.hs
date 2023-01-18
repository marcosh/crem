{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module CRM.StateMachine where

import CRM.BaseMachine as BaseMachine
import CRM.Topology
import "base" Control.Category (Category (..))
import "base" Data.Bifunctor (bimap)
import "base" Data.Kind (Type)
import "profunctors" Data.Profunctor (Choice (..), Profunctor (..), Strong (..))
import "singletons-base" Data.Singletons (Demote, SingI, SingKind)
import Prelude hiding ((.))

-- | A `StateMachine` is a [Mealy machine](https://en.wikipedia.org/wiki/Mealy_machine)
-- with inputs of type `input` and outputs of type `output`
data StateMachine input output where
  Basic
    :: forall vertex (topology :: Topology vertex) input output
     . ( Demote (Topology vertex) ~ Topology vertex
       , SingKind vertex
       , SingI topology
       , Show vertex
       )
    => BaseMachine topology input output
    -> StateMachine input output
  Compose
    :: StateMachine a b
    -> StateMachine b c
    -> StateMachine a c
  Parallel
    :: StateMachine a b
    -> StateMachine c d
    -> StateMachine (a, c) (b, d)
  Alternative
    :: StateMachine a b
    -> StateMachine c d
    -> StateMachine (Either a c) (Either b d)

-- | a state machine which does not rely on state
stateless :: (a -> b) -> StateMachine a b
stateless f = Basic $ statelessBase f

-- | a machine modelled with explicit state, where every transition is allowed
unrestrictedMachine
  :: (Demote vertex ~ vertex, SingKind vertex, SingI (AllowAllTopology @vertex), Show vertex)
  => ( forall initialVertex
        . state initialVertex
       -> a
       -> ActionResult (AllowAllTopology @vertex) state initialVertex b
     )
  -> InitialState (state :: vertex -> Type)
  -> StateMachine a b
unrestrictedMachine action state = Basic $ unrestrictedBaseMachine action state

-- * Category

instance Category StateMachine where
  id :: StateMachine a a
  id = Basic identity

  (.) :: StateMachine b c -> StateMachine a b -> StateMachine a c
  (.) = flip Compose

-- * Profunctor

instance Profunctor StateMachine where
  lmap :: (a -> b) -> StateMachine b c -> StateMachine a c
  lmap f (Basic baseMachine) = Basic $ lmap f baseMachine
  lmap f (Compose machine1 machine2) = Compose (lmap f machine1) machine2
  lmap f machine = Compose (stateless f) machine


  rmap :: (b -> c) -> StateMachine a b -> StateMachine a c
  rmap f (Basic baseMachine) = Basic $ rmap f baseMachine
  rmap f (Compose machine1 machine2) = Compose machine1 (rmap f machine2)
  rmap f machine = Compose machine (stateless f)

-- * Strong

instance Strong StateMachine where
  first' :: StateMachine a b -> StateMachine (a, c) (b, c)
  first' = flip Parallel Control.Category.id

  second' :: StateMachine a b -> StateMachine (c, a) (c, b)
  second' = Parallel Control.Category.id

-- * Choice
-- | An instance of `Choice` allows us to have parallel composition of state machines, meaning that we can pass two inputs to two state machines and get out the outputs of both
instance Choice StateMachine where
  left' :: StateMachine a b -> StateMachine (Either a c) (Either b c)
  left' = flip Alternative Control.Category.id

  right' :: StateMachine a b -> StateMachine (Either c a) (Either c b)
  right' = Alternative Control.Category.id

-- * Run a state machine

-- | Given an `input`, run the machine to get an output and a new version of
-- the machine
run :: StateMachine a b -> a -> (b, StateMachine a b)
run (Basic baseMachine) a = Basic <$> runBaseMachine baseMachine a
run (Compose machine1 machine2) a =
  let
    (output1, machine1') = run machine1 a
    (output2, machine2') = run machine2 output1
   in
    (output2, Compose machine1' machine2')
run (Parallel machine1 machine2) (a, b) =
  let
    (output1, machine1') = run machine1 a
    (output2, machine2') = run machine2 b
   in
    ((output1, output2), Parallel machine1' machine2')
run (Alternative machine1 machine2) a =
  case a of
    Left a1 -> bimap Left (`Alternative` machine2) $ run machine1 a1
    Right a2 -> bimap Right (machine1 `Alternative`) $ run machine2 a2
