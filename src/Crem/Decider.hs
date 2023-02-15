{-# LANGUAGE DataKinds #-}

-- | The [Decider pattern](https://thinkbeforecoding.com/post/2021/12/17/functional-event-sourcing-decider)
-- allows to easily describe an [aggregate](https://www.domainlanguage.com/wp-content/uploads/2016/05/DDD_Reference_2015-03.pdf)
-- in functional terms
--
-- In terms of Mealy machines, a `Decider` is a machine where the next state is
-- computed from the previous state and the output
module Crem.Decider where

import Crem.BaseMachine (ActionResult (..), BaseMachine, BaseMachineT (..), InitialState (..))
import Crem.Topology (AllowedTransition, Topology)
import Data.Foldable (foldl')
import "base" Data.Kind (Type)

-- | A @Decider topology input output@ is a Decider which receives inputs of
-- type @input@ and emits outputs of type @output@, where allowed transitions
-- are constrained by the provided @topology@.
--
-- Being used to describe the domain logic of an aggregate, a `Decider` is
-- always pure.
--
-- It is defined by:
--
--   * its `deciderInitialState`
--   * a `decide` function, which says how to compute the @output@ out of the
-- @input@ and the current state
--   * an `evolve` function, which allows us to specify the next state from the
-- current state and the @output@
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

-- | A smart wrapper over the machine state, which allows to enforce that only
-- transitions allowed by the @topology@ are actually performed.
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
-- This is the main selling point of a `Decider` over a generic `Crem.StateMachine`,
-- since it allows rebuilding a machine from its outputs.
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
