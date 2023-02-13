{-# LANGUAGE GADTs #-}

module Crem.Example.PlusOneUpToFour where

import Crem.StateMachine (StateMachine, stateless)

plus1UpTo4 :: StateMachine Int [Int]
plus1UpTo4 =
  stateless (\i -> [i + 1 | i < 5])
