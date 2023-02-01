{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module CRM.Example.TriangularMachine where

import CRM.BaseMachine (InitialState (..), pureResult)
import CRM.StateMachine (StateMachine, unrestrictedMachine)

data TriangularState (a :: ()) where
  OnlyState :: Int -> TriangularState '()

triangular :: StateMachine Int Int
triangular =
  unrestrictedMachine
    ( \case
        OnlyState state ->
          \input ->
            pureResult
              (state + input)
              (OnlyState $ state + 1)
    )
    (InitialState (OnlyState 0))
