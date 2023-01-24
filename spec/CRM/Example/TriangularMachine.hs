{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module CRM.Example.TriangularMachine where

import CRM.BaseMachine (ActionResult (..), InitialState (..))
import CRM.StateMachine (StateMachine, unrestrictedMachine)

data TriangularState (a :: ()) where
  OnlyState :: Int -> TriangularState '()

triangular :: StateMachine Int Int
triangular =
  unrestrictedMachine
    ( \case
        OnlyState state ->
          \input ->
            ActionResult
              (OnlyState $ state + 1)
              (state + input)
    )
    (InitialState (OnlyState 0))
