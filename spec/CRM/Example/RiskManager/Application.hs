module CRM.Example.RiskManager.Application where

import CRM.Example.RiskManager.Aggregate (riskAggregate)
import CRM.Example.RiskManager.Domain (RiskCommand, RiskEvent)
import CRM.Example.RiskManager.Policy (riskPolicy)
import CRM.Example.RiskManager.Projection (ReceivedData, riskProjection)
import "crm" CRM.StateMachine
import "base" Data.List (singleton)
import "base" Data.Maybe (maybeToList)
import "profunctors" Data.Profunctor (rmap)
import Prelude hiding ((.))

aggregate :: StateMachine RiskCommand [RiskEvent]
aggregate = rmap maybeToList $ Basic riskAggregate

policy :: StateMachine RiskEvent [RiskCommand]
policy = rmap maybeToList riskPolicy

circle :: StateMachine RiskEvent [RiskEvent]
circle = Kleisli policy aggregate

loop :: StateMachine RiskEvent [RiskEvent]
loop = Loop circle

writeModel :: StateMachine RiskCommand [RiskEvent]
writeModel = Kleisli aggregate loop

projection :: StateMachine RiskEvent ReceivedData
projection = Basic riskProjection

readModel :: StateMachine RiskEvent [ReceivedData]
readModel = rmap singleton projection

whole :: StateMachine RiskCommand [ReceivedData]
whole = Kleisli writeModel readModel

riskApplication :: StateMachine RiskCommand ReceivedData
riskApplication = rmap mconcat whole
