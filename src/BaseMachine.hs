{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module BaseMachine () where

import "base" Data.Kind (Type)
import "singletons-base" Data.Singletons.Base.TH (singletons)

{- | A `Topology` is a description of the topology of a state machine
   It contains the collection of allowed transitions
-}
$( singletons
    [d|
      newtype Topology (vertex :: Type) = MkTopology {edges :: [(vertex, [vertex])]}
      |]
 )
