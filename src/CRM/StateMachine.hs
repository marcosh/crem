{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module CRM.StateMachine where

import CRM.BaseMachine
import CRM.Topology
import "profunctors" Data.Profunctor (Profunctor (..))
import "singletons-base" Data.Singletons (Demote, SingI, SingKind)

-- import "base" Control.Category (Category (..))

{- | A `StateMachine` is a [Mealy machine](https://en.wikipedia.org/wiki/Mealy_machine)
 with inputs of type `input` and outputs of type `output`
-}
data StateMachine input output where
  Basic
    :: forall vertex (topology :: Topology vertex) input output
     . (Demote (Topology vertex) ~ Topology vertex, SingKind vertex, SingI topology, Show vertex)
    => BaseMachine topology input output
    -> StateMachine input output
  Compose
    :: StateMachine a b
    -> StateMachine b c
    -> StateMachine a c

-- * SemiCategory

infixr 1 >>>
infixr 1 <<<

(>>>) :: StateMachine a b -> StateMachine b c -> StateMachine a c
(>>>) = Compose

(<<<) :: StateMachine b c -> StateMachine a b -> StateMachine a c
(<<<) = flip (>>>)

-- * Profunctor

instance Profunctor StateMachine where
  lmap :: (a -> b) -> StateMachine b c -> StateMachine a c
  lmap f (Basic baseMachine) = Basic $ lmap f baseMachine
  lmap f (Compose machine1 machine2) = Compose (lmap f machine1) machine2

  rmap :: (b -> c) -> StateMachine a b -> StateMachine a c
  rmap f (Basic baseMachine) = Basic $ rmap f baseMachine
  rmap f (Compose machine1 machine2) = Compose machine1 (rmap f machine2)

-- * Strong

-- * Choice
