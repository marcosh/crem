module Crem.Example.Cart.Application where

import Crem.Example.Cart.Aggregate
import Crem.Example.Cart.Domain
import Crem.Example.Cart.Policy
import Crem.Example.Cart.Projection
import Crem.StateMachine

writeModel :: StateMachine CartCommand [CartEvent]
writeModel = Feedback cart paymentGateway

application :: StateMachine CartCommand [String]
application = Kleisli writeModel cartState
