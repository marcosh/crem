{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module CRM.Example.OneState where

import "crm" CRM.BaseMachine
import "crm" CRM.Topology
import "singletons-base" Data.Singletons.Base.TH

oneVertexMachine :: BaseMachine (TrivialTopology @()) () ()
oneVertexMachine =
  BaseMachine
    { initialState = InitialState STuple0
    , action = \STuple0 _ -> ActionResult STuple0 ()
    }
