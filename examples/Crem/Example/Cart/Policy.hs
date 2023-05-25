module Crem.Example.Cart.Policy where

import Crem.Example.Cart.Domain
import Crem.StateMachine

paymentGateway :: StateMachine CartEvent [CartCommand]
paymentGateway = stateless $ \case
  CartPaymentInitiated -> [MarkCartAsPaid] -- in this word payments always succeed
  CartPaymentCompleted -> []
