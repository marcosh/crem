module Crem.Example.Cart.Projection where

import Crem.Example.Cart.Domain
import Crem.StateMachine

cartState :: StateMachine CartEvent [String]
cartState = stateless $ \case
  CartPaymentInitiated -> ["initiated"]
  CartPaymentCompleted -> ["completed"]
