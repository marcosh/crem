{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RoleAnnotations #-}

module Crem.Example.TriangularMachine where

import Crem.BaseMachine (InitialState (..), pureResult)
import Crem.StateMachine (StateMachine, unrestrictedMachine)

data TriangularState (a :: ()) where
  OnlyState :: Int -> TriangularState '()

type role TriangularState nominal

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
