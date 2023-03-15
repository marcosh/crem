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

module Crem.Example.RiskManager.Aggregate where

import Crem.BaseMachine
import Crem.Example.RiskManager.Domain
import Crem.Render.RenderableVertices (AllVertices (..), RenderableVertices)
import Crem.Topology
import "singletons-base" Data.Singletons.Base.TH

$( singletons
    [d|
      data AggregateVertex
        = NoDataVertex
        | CollectedUserDataVertex
        | CollectedLoanDetailsFirstVertex
        | ReceivedCreditBureauDataFirstVertex
        | CollectedAllDataVertex
        deriving stock (Eq, Show, Enum, Bounded)

      aggregateTopology :: Topology AggregateVertex
      aggregateTopology =
        Topology
          [ (NoDataVertex, [CollectedUserDataVertex])
          , (CollectedUserDataVertex, [CollectedLoanDetailsFirstVertex, ReceivedCreditBureauDataFirstVertex])
          , (CollectedLoanDetailsFirstVertex, [CollectedAllDataVertex])
          , (ReceivedCreditBureauDataFirstVertex, [CollectedAllDataVertex])
          , (CollectedAllDataVertex, [])
          ]
      |]
 )

deriving via AllVertices AggregateVertex instance RenderableVertices AggregateVertex

data AggregateState (vertex :: AggregateVertex) where
  NoData :: AggregateState 'NoDataVertex
  CollectedUserData :: UserData -> AggregateState 'CollectedUserDataVertex
  CollectedLoanDetailsFirst :: UserData -> LoanDetails -> AggregateState 'CollectedLoanDetailsFirstVertex
  ReceivedCreditBureauDataFirst :: UserData -> CreditBureauData -> AggregateState 'ReceivedCreditBureauDataFirstVertex
  CollectedAllData :: UserData -> LoanDetails -> CreditBureauData -> AggregateState 'CollectedAllDataVertex

riskAggregate :: BaseMachine AggregateTopology RiskCommand (Maybe RiskEvent)
riskAggregate =
  BaseMachineT
    { initialState = InitialState NoData
    , action = \case
        NoData -> \case
          RegisterUserData ud -> pureResult (Just $ UserDataRegistered ud) (CollectedUserData ud)
          _ -> pureResult Nothing NoData
        CollectedUserData ud -> \case
          RegisterUserData ud' -> pureResult (Just $ UserDataRegistered ud') (CollectedUserData ud')
          ProvideLoanDetails ld -> pureResult (Just $ LoanDetailsProvided ld) (CollectedLoanDetailsFirst ud ld)
          ProvideCreditBureauData cbd -> pureResult (Just $ CreditBureauDataReceived cbd) (ReceivedCreditBureauDataFirst ud cbd)
        CollectedLoanDetailsFirst ud ld -> \case
          RegisterUserData ud' -> pureResult (Just $ UserDataRegistered ud') (CollectedLoanDetailsFirst ud' ld)
          ProvideLoanDetails ld' -> pureResult (Just $ LoanDetailsProvided ld') (CollectedLoanDetailsFirst ud ld')
          ProvideCreditBureauData cbd -> pureResult (Just $ CreditBureauDataReceived cbd) (CollectedAllData ud ld cbd)
        ReceivedCreditBureauDataFirst ud cbd -> \case
          RegisterUserData ud' -> pureResult (Just $ UserDataRegistered ud') (ReceivedCreditBureauDataFirst ud' cbd)
          ProvideLoanDetails ld -> pureResult (Just $ LoanDetailsProvided ld) (CollectedAllData ud ld cbd)
          ProvideCreditBureauData cbd' -> pureResult (Just $ CreditBureauDataReceived cbd') (ReceivedCreditBureauDataFirst ud cbd')
        CollectedAllData ud ld cbd -> \case
          RegisterUserData ud' -> pureResult (Just $ UserDataRegistered ud') (CollectedAllData ud' ld cbd)
          ProvideLoanDetails ld' -> pureResult (Just $ LoanDetailsProvided ld') (CollectedAllData ud ld' cbd)
          ProvideCreditBureauData cbd' -> pureResult (Just $ CreditBureauDataReceived cbd') (CollectedAllData ud ld cbd')
    }
