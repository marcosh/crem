{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Crem.Example.OneState where

import Crem.BaseMachine
import Crem.Topology
import "singletons-base" Data.Singletons.Base.TH

oneVertexMachine :: BaseMachine (TrivialTopology @()) () ()
oneVertexMachine =
  BaseMachineT
    { initialState = InitialState STuple0
    , action = \STuple0 _ -> pureResult () STuple0
    }
