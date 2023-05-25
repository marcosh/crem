module Crem.Example.Cart.Domain where

data CartCommand
  = PayCart
  | MarkCartAsPaid

data CartEvent
  = CartPaymentInitiated
  | CartPaymentCompleted
