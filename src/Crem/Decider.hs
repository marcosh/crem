{-# LANGUAGE DataKinds #-}

module Crem.Decider where

import Crem.BaseMachine (ActionResult (..), BaseMachineT (..), InitialState)
import Crem.Topology (AllowedTransition, Topology)
import "base" Data.Kind (Type)

-- | The [Decider pattern](https://thinkbeforecoding.com/post/2021/12/17/functional-event-sourcing-decider)
-- allows to easily describe an aggregate in functional terms
--
-- In terms of Mealy machine, the next state is computed by the previous state
-- and the output
data
  DeciderT
    m
    (topology :: Topology vertex)
    input
    output = forall state.
  DeciderT
  { deciderInitialState :: InitialState state
  , decide :: forall vertex'. input -> state vertex' -> output
  , evolve
      :: forall initialVertex
       . state initialVertex
      -> output
      -> EvolutionResult m topology state initialVertex output
  }

data
  EvolutionResult
    m
    (topology :: Topology vertex)
    (state :: vertex -> Type)
    (initialVertex :: vertex)
    output
  where
  EvolutionResult
    :: AllowedTransition topology initialVertex finalVertex
    => m (state finalVertex)
    -> EvolutionResult m topology state initialVertex output

deciderMachine
  :: Functor m
  => DeciderT m topology input output
  -> BaseMachineT m topology input output
deciderMachine (DeciderT deciderInitialState' decide' evolve') =
  BaseMachineT
    { initialState = deciderInitialState'
    , action = \state input ->
        let
          output = decide' input state
         in
          case evolve' state output of
            EvolutionResult mFinalState ->
              ActionResult $ (output,) <$> mFinalState
    }
