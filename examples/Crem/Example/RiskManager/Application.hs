module Crem.Example.RiskManager.Application where

import Crem.Example.RiskManager.Aggregate (riskAggregate)
import Crem.Example.RiskManager.Domain (RiskCommand, RiskEvent)
import Crem.Example.RiskManager.Policy (riskPolicy)
import Crem.Example.RiskManager.Projection (ReceivedData, riskProjection)
import Crem.StateMachine
import "base" Data.List (singleton)
import "base" Data.Maybe (maybeToList)
import "profunctors" Data.Profunctor (rmap)
import Prelude hiding ((.))

aggregate :: StateMachine RiskCommand [RiskEvent]
aggregate = rmap maybeToList $ Basic riskAggregate

policy :: StateMachine RiskEvent [RiskCommand]
policy = rmap maybeToList riskPolicy

writeModel :: StateMachine RiskCommand [RiskEvent]
writeModel = Feedback aggregate policy

projection :: StateMachine RiskEvent ReceivedData
projection = Basic riskProjection

readModel :: StateMachine RiskEvent [ReceivedData]
readModel = rmap singleton projection

whole :: StateMachine RiskCommand [ReceivedData]
whole = Kleisli writeModel readModel

riskApplication :: StateMachine RiskCommand ReceivedData
riskApplication = rmap mconcat whole
