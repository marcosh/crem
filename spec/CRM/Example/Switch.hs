{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module CRM.Example.Switch where

import "crm" CRM.BaseMachine
import "singletons-base" Data.Singletons.Base.TH

$( singletons
    [d|
      -- topology with a two vertices and one edge from each vertex to the
      -- other
      switchTopology :: Topology Bool
      switchTopology =
        MkTopology
          [ (True, [False])
          , (False, [True])
          ]
      |]
 )

switchMachine :: StateMachine SwitchTopology () ()
switchMachine =
  MkStateMachine
    { initialState = MkInitialState SFalse
    , action = \case
        SFalse () -> MkActionResult STrue ()
        STrue () -> MkActionResult SFalse ()
    }
