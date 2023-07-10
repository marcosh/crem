{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag--Wmissing-deriving-strategies
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
-- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag--Wredundant-constraints
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
-- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag--Wunused-type-patterns
{-# OPTIONS_GHC -Wno-unused-type-patterns #-}

module Crem.Example.Cart.Shipping where

import Crem.BaseMachine
import Crem.Example.Cart.Application
import Crem.Example.Cart.Domain
import Crem.Example.Cart.Projection
import Crem.Render.RenderableVertices
import Crem.StateMachine
import Crem.Topology
import "base" Control.Arrow hiding (Kleisli)
import "profunctors" Data.Profunctor
import "singletons-base" Data.Singletons.Base.TH

data ShippingCommand
  = StartShipping

data ShippingEvent

$( singletons
    [d|
      data ShippingVertex = ShippingVertex
        deriving stock (Eq, Show, Enum, Bounded)

      shippingTopology :: Topology ShippingVertex
      shippingTopology = Topology []
    |]
 )

deriving via AllVertices ShippingVertex instance RenderableVertices ShippingVertex

shippingBasic :: BaseMachine ShippingTopology ShippingCommand [ShippingEvent]
shippingBasic = undefined

shipping :: StateMachine ShippingCommand [ShippingEvent]
shipping = Basic shippingBasic

writeModelWithShipping :: StateMachine (Either CartCommand ShippingCommand) [Either CartEvent ShippingEvent]
writeModelWithShipping = rmap (fmap Left ||| fmap Right) $ writeModel +++ shipping

paymentCompletePolicy :: StateMachine CartEvent [ShippingCommand]
paymentCompletePolicy = stateless $ \case
  CartPaymentInitiated -> []
  CartPaymentCompleted -> [StartShipping]

writeModelWithShipping' :: StateMachine (Either CartCommand ShippingCommand) [Either CartEvent ShippingEvent]
writeModelWithShipping' = Feedback
  writeModelWithShipping
  (rmap (fmap Right) paymentCompletePolicy ||| stateless (const []))

data ShippingInfo

shippingInfo :: StateMachine ShippingEvent [ShippingInfo]
shippingInfo = undefined

readModel :: StateMachine (Either CartEvent ShippingEvent) [Either CartView ShippingInfo]
readModel = rmap (fmap Left ||| fmap Right) $ paymentStatus +++ shippingInfo

cartAndShipping :: StateMachine (Either CartCommand ShippingCommand) [Either CartView ShippingInfo]
cartAndShipping = Kleisli writeModelWithShipping' readModel
