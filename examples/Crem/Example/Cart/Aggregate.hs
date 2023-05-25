{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag--Wmissing-deriving-strategies
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
-- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag--Wunticked-promoted-constructors
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
-- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag--Wunused-type-patterns
{-# OPTIONS_GHC -Wno-unused-type-patterns #-}

module Crem.Example.Cart.Aggregate where

import Crem.BaseMachine
import Crem.Example.Cart.Domain
import Crem.Render.RenderableVertices
import Crem.StateMachine
import Crem.Topology
import "singletons-base" Data.Singletons.Base.TH

$( singletons
    [d|
      data CartVertex
        = WaitingForPayment
        | InitiatingPayment
        | PaymentComplete
        deriving stock (Eq, Show, Enum, Bounded)

      cartTopology :: Topology CartVertex
      cartTopology =
        Topology
          [ (WaitingForPayment, [InitiatingPayment])
          , (InitiatingPayment, [PaymentComplete])
          , (PaymentComplete, [])
          ]
      |]
 )

deriving via AllVertices CartVertex instance RenderableVertices CartVertex

data CartState (cartVertex :: CartVertex) where
  WaitingForPaymentState :: CartState WaitingForPayment
  InitiatingPaymentState :: CartState InitiatingPayment
  PaymentCompleteState :: CartState PaymentComplete

cartBasic :: BaseMachine CartTopology CartCommand [CartEvent]
cartBasic =
  BaseMachineT
    { initialState = InitialState WaitingForPaymentState
    , action = \case
        WaitingForPaymentState -> \case
          PayCart -> pureResult [CartPaymentInitiated] InitiatingPaymentState
          MarkCartAsPaid -> pureResult [] WaitingForPaymentState
        InitiatingPaymentState -> \case
          PayCart -> pureResult [] InitiatingPaymentState
          MarkCartAsPaid -> pureResult [CartPaymentCompleted] PaymentCompleteState
        PaymentCompleteState -> \_ -> pureResult [] PaymentCompleteState
    }

cart :: StateMachine CartCommand [CartEvent]
cart = Basic cartBasic
