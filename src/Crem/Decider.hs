{-# LANGUAGE DataKinds #-}

module Crem.Decider where

import Crem.BaseMachine (ActionResult (..), BaseMachine, BaseMachineT (..), InitialState)
import Crem.Topology (AllowedTransition, Topology)
import "base" Data.Kind (Type)

-- | The [Decider pattern](https://thinkbeforecoding.com/post/2021/12/17/functional-event-sourcing-decider)
-- allows to easily describe an aggregate in functional terms
--
-- In terms of Mealy machine, the next state is computed by the previous state
-- and the output
data
  Decider
    (topology :: Topology vertex)
    input
    output = forall state.
  Decider
  { deciderInitialState :: InitialState state
  , decide :: forall vertex'. input -> state vertex' -> output
  , evolve
      :: forall initialVertex
       . state initialVertex
      -> output
      -> EvolutionResult topology state initialVertex output
  }

data
  EvolutionResult
    (topology :: Topology vertex)
    (state :: vertex -> Type)
    (initialVertex :: vertex)
    output
  where
  EvolutionResult
    :: AllowedTransition topology initialVertex finalVertex
    => state finalVertex
    -> EvolutionResult topology state initialVertex output

deciderMachine
  :: Decider topology input output
  -> BaseMachine topology input output
deciderMachine (Decider deciderInitialState' decide' evolve') =
  BaseMachineT
    { initialState = deciderInitialState'
    , action = \state input ->
        let
          output = decide' input state
         in
          case evolve' state output of
            EvolutionResult finalState ->
              ActionResult $ pure (output, finalState)
    }
