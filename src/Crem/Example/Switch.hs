{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag--Wall-missed-specialisations
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Crem.Example.Switch where

import Crem.BaseMachine
import Crem.Topology
import "singletons-base" Data.Singletons.Base.TH

$( singletons
    [d|
      -- topology with a two vertices and one edge from each vertex to the
      -- other
      switchTopology :: Topology Bool
      switchTopology =
        Topology
          [ (True, [False])
          , (False, [True])
          ]
      |]
 )

switchMachine :: SBool a -> BaseMachine SwitchTopology () ()
switchMachine initialState =
  BaseMachineT
    { initialState = InitialState initialState
    , action = \case
        SFalse -> \case
          () -> pureResult () STrue
        STrue -> \case
          () -> pureResult () SFalse
    }
