module Crem.Example.Cart.Projection where

import Crem.Example.Cart.Domain
import Crem.StateMachine

data CartView
  = Initiated
  | Completed

paymentStatus :: StateMachine CartEvent [CartView]
paymentStatus = stateless $ \case
  CartPaymentInitiated -> [Initiated]
  CartPaymentCompleted -> [Completed]
