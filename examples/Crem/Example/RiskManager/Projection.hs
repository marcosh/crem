{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
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

module Crem.Example.RiskManager.Projection where

import Crem.BaseMachine
import Crem.Example.RiskManager.Domain
import Crem.Topology
import "base" Data.Monoid (Last (..))
import "base" GHC.Generics (Generic)
import "singletons-base" Data.Singletons.Base.TH

data ReceivedData = ReceivedData
  { receivedUserData :: Maybe UserData
  , receivedLoanDetails :: Maybe LoanDetails
  , receivedCreditBureauData :: Maybe CreditBureauData
  }
  deriving stock (Eq, Show, Generic)

instance Semigroup ReceivedData where
  (<>) :: ReceivedData -> ReceivedData -> ReceivedData
  r1 <> r2 =
    ReceivedData
      { receivedUserData =
          getLast $ Last (receivedUserData r1) <> Last (receivedUserData r2)
      , receivedLoanDetails =
          getLast $ Last (receivedLoanDetails r1) <> Last (receivedLoanDetails r2)
      , receivedCreditBureauData =
          getLast $ Last (receivedCreditBureauData r1) <> Last (receivedCreditBureauData r2)
      }

instance Monoid ReceivedData where
  mempty :: ReceivedData
  mempty =
    ReceivedData
      { receivedUserData = Nothing
      , receivedLoanDetails = Nothing
      , receivedCreditBureauData = Nothing
      }

$( singletons
    [d|
      data ProjectionVertex
        = SingleProjectionVertex
        deriving stock (Eq, Show, Enum, Bounded)

      projectionTopology :: Topology ProjectionVertex
      projectionTopology =
        Topology []
      |]
 )

data ProjectionState (vertex :: ProjectionVertex) where
  SingleProjectionState :: ReceivedData -> ProjectionState 'SingleProjectionVertex

riskProjection :: BaseMachine ProjectionTopology RiskEvent ReceivedData
riskProjection =
  BaseMachineT
    { initialState = InitialState (SingleProjectionState mempty)
    , action = \(SingleProjectionState receivedData) input ->
        let
          newReceivedData = case input of
            UserDataRegistered ud -> receivedData {receivedUserData = Just ud}
            LoanDetailsProvided ld -> receivedData {receivedLoanDetails = Just ld}
            CreditBureauDataReceived cbd -> receivedData {receivedCreditBureauData = Just cbd}
         in
          pureResult newReceivedData (SingleProjectionState newReceivedData)
    }
