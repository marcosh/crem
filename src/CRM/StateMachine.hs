{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module CRM.StateMachine where

import CRM.BaseMachine
import CRM.Topology
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

-- * Strong

-- * Choice
