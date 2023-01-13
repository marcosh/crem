{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module CRM.Example.OneState where

import "crm" CRM.BaseMachine
import "crm" CRM.Topology
import "singletons-base" Data.Singletons.Base.TH

$( singletons
    [d|
      -- topology with a single vertex and one edge from the vertex to itself
      singleVertexTopology :: Topology ()
      singleVertexTopology = Topology [((), [()])]
      |]
 )

oneVertexMachine :: BaseMachine SingleVertexTopology () ()
oneVertexMachine =
  BaseMachine
    { action = \STuple0 _ -> ActionResult STuple0 ()
    }
