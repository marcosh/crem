{-# LANGUAGE DataKinds #-}

module Crem.Decider where

import Crem.BaseMachine (ActionResult (..), BaseMachine, BaseMachineT (..), InitialState (..))
import Crem.Topology (AllowedTransition, Topology)
import Data.Foldable (foldl')
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

-- | translate a `Decider` into a `BaseMachine`
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

-- | rebuild a `Decider` from a list of outputs
--
-- This is the main selling point of a `Decider` over a generic `StateMachine`,
-- since it allows it to be rebuilt from its outputs.
rebuildDecider
  :: [output]
  -> Decider topology input output
  -> Decider topology input output
rebuildDecider outputs decider =
  foldl' rebuildDeciderStep decider outputs
  where
    rebuildDeciderStep
      :: Decider topology input output
      -> output
      -> Decider topology input output
    rebuildDeciderStep (Decider (InitialState initialState') decide' evolve') output =
      let
        evolveResult = evolve' initialState' output
       in
        case evolveResult of
          EvolutionResult evolvedState ->
            Decider
              { deciderInitialState = InitialState evolvedState
              , decide = decide'
              , evolve = evolve'
              }
