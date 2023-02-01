{-# LANGUAGE GADTs #-}

module CRM.Example.PlusOneUpToFour where

import CRM.StateMachine (StateMachine, stateless)

plus1UpTo4 :: StateMachine Int [Int]
plus1UpTo4 =
  stateless (\i -> [i + 1 | i < 5])
