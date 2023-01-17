{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module CRM.StateMachine where

import CRM.BaseMachine as BaseMachine
import CRM.Topology
import "base" Control.Category (Category (..))
import "profunctors" Data.Profunctor (Choice (..), Profunctor (..), Strong (..))
import "singletons-base" Data.Singletons (Demote, SingI, SingKind)

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

  rmap :: (b -> c) -> StateMachine a b -> StateMachine a c
  rmap f (Basic baseMachine) = Basic $ rmap f baseMachine
  rmap f (Compose machine1 machine2) = Compose machine1 (rmap f machine2)

-- * Strong

instance Strong StateMachine where
  first' :: StateMachine a b -> StateMachine (a, c) (b, c)
  first' (Basic baseMachine) = Basic $ first' baseMachine
  first' (Compose machine1 machine2) = Compose (first' machine1) (first' machine2)

  second' :: StateMachine a b -> StateMachine (c, a) (c, b)
  second' (Basic baseMachine) = Basic $ second' baseMachine
  second' (Compose machine1 machine2) = Compose (second' machine1) (second' machine2)

-- * Choice
-- | An instance of `Choice` allows us to have parallel composition of state machines, meaning that we can pass two inputs to two state machines and get out the outputs of both
instance Choice StateMachine where
  left' :: StateMachine a b -> StateMachine (Either a c) (Either b c)
  left' (Basic baseMachine) = Basic $ left' baseMachine
  left' (Compose machine1 machine2) = Compose (left' machine1) (left' machine2)

  right' :: StateMachine a b -> StateMachine (Either c a) (Either c b)
  right' (Basic baseMachine) = Basic $ right' baseMachine
  right' (Compose machine1 machine2) = Compose (right' machine1) (right' machine2)

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
