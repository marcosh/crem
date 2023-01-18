{-# LANGUAGE GADTs #-}

module CRM.Example.BooleanStateMachine where

import "crm" CRM.BaseMachine
import "crm" CRM.StateMachine
import "singletons-base" Data.Singletons.Base.TH

booleanStateMachine
  :: forall a
   . SBool a
  -> StateMachine Int Int
booleanStateMachine initialState =
  unrestrictedMachine @Bool
    ( \state input -> case state of
        SFalse ->
          if even input
            then ActionResult SFalse (input + 1)
            else ActionResult STrue (input * 3)
        STrue ->
          if even input
            then ActionResult STrue (input - 1)
            else ActionResult SFalse (input * 5)
    )
    (InitialState initialState)
