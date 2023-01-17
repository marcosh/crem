{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag--Wall-missed-specialisations
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module CRM.Example.Switch where

import "crm" CRM.BaseMachine
import "crm" CRM.Topology
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
  BaseMachine
    { initialState = InitialState initialState
    , action = \case
        SFalse -> \case
          () -> ActionResult STrue ()
        STrue -> \case
          () -> ActionResult SFalse ()
    }
