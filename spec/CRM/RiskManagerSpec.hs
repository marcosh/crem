module CRM.RiskManagerSpec where

import CRM.Example.RiskManager.Application
import CRM.Example.RiskManager.Domain
import CRM.Example.RiskManager.Projection (ReceivedData (..))
import "crm" CRM.StateMachine (run, runMultiple)
import CRM.StateMachineSpec (shouldOutput)
import "hspec" Test.Hspec (Spec, describe, it)

myUserData :: UserData
myUserData =
  UserData
    { name = "Marco"
    , surname = "Perone"
    , taxCode = "ABCDEF12T34J987H"
    }

notMyUserData :: UserData
notMyUserData =
  UserData
    { name = "Gigi"
    , surname = "Zucon"
    , taxCode = "ABCDEF12T34J987H"
    }

loanDetails :: LoanDetails
loanDetails =
  LoanDetails
    { amount = EuroCents 10000
    , instalments = InstalmentsNumber 10
    }

otherLoanDetails :: LoanDetails
otherLoanDetails =
  LoanDetails
    { amount = EuroCents 20000
    , instalments = InstalmentsNumber 5
    }

creditBureauData :: CreditBureauData
creditBureauData =
  CreditBureauData
    { missedPaymentDeadlines = MissedPaymentDeadlines 2
    , arrears = EuroCents 100000
    }

spec :: Spec
spec =
  describe "RiskManager" $ do
    describe "aggregate" $ do
      it "emits a registered event when a registration command is received" $ do
        run aggregate (RegisterUserData myUserData)
          `shouldOutput` [UserDataRegistered myUserData]

      it "emits two registered events when two registration commands are received" $ do
        runMultiple aggregate [RegisterUserData myUserData, RegisterUserData notMyUserData]
          `shouldOutput` [UserDataRegistered myUserData, UserDataRegistered notMyUserData]

      it "does not emit anything when a loan details command is received" $ do
        run aggregate (ProvideLoanDetails loanDetails)
          `shouldOutput` []

      it "emits a loan details event when a registration command and a loan detail command are received" $ do
        runMultiple aggregate [RegisterUserData myUserData, ProvideLoanDetails loanDetails]
          `shouldOutput` [UserDataRegistered myUserData, LoanDetailsProvided loanDetails]

    describe "policy" $ do
      it "emits a credit bureau command when a user registration event is received for a user named Marco" $ do
        run policy (UserDataRegistered myUserData)
          `shouldOutput` [ProvideCreditBureauData creditBureauData]

      it "emits no command when a user registration event is received for a user not named Marco" $ do
        run policy (UserDataRegistered notMyUserData)
          `shouldOutput` []

      it "emits two credit bureau command when two user registration events are received" $ do
        runMultiple policy [UserDataRegistered myUserData, UserDataRegistered myUserData]
          `shouldOutput` [ ProvideCreditBureauData creditBureauData
                         , ProvideCreditBureauData creditBureauData
                         ]

      it "emits one credit bureau command when two user registration events are received but only one for a user named Marco" $ do
        runMultiple policy [UserDataRegistered myUserData, UserDataRegistered notMyUserData]
          `shouldOutput` [ProvideCreditBureauData creditBureauData]

        runMultiple policy [UserDataRegistered notMyUserData, UserDataRegistered myUserData]
          `shouldOutput` [ProvideCreditBureauData creditBureauData]

      it "emits no command when two user registration events are received for a user not named Marco" $ do
        runMultiple policy [UserDataRegistered notMyUserData, UserDataRegistered notMyUserData]
          `shouldOutput` []

      it "emits no command when a loan details event is registered" $ do
        run policy (LoanDetailsProvided loanDetails)
          `shouldOutput` []

      it "emits no command when a credit bureau data is registered" $ do
        run policy (CreditBureauDataReceived creditBureauData)
          `shouldOutput` []

    describe "writeModel" $ do
      it "emits a registered event when a registration command is received" $ do
        run writeModel (RegisterUserData myUserData)
          `shouldOutput` [ UserDataRegistered myUserData
                         , CreditBureauDataReceived creditBureauData
                         ]

      it "emits two registered events when two registration command are received" $ do
        runMultiple writeModel [RegisterUserData myUserData, RegisterUserData myUserData]
          `shouldOutput` [ UserDataRegistered myUserData
                         , CreditBureauDataReceived creditBureauData
                         , UserDataRegistered myUserData
                         , CreditBureauDataReceived creditBureauData
                         ]

    describe "riskProjection" $ do
      it "registers one user when a registration event is received" $ do
        run projection (UserDataRegistered myUserData)
          `shouldOutput` ReceivedData
            { receivedUserData = Just myUserData
            , receivedLoanDetails = Nothing
            , receivedCreditBureauData = Nothing
            }

      it "updates the user data when two registration events are received" $ do
        runMultiple projection [UserDataRegistered myUserData, UserDataRegistered notMyUserData]
          `shouldOutput` ReceivedData
            { receivedUserData = Just notMyUserData
            , receivedLoanDetails = Nothing
            , receivedCreditBureauData = Nothing
            }

    describe "readModel" $ do
      it "registers one user when a registration event is received" $ do
        run readModel (UserDataRegistered myUserData)
          `shouldOutput` [ ReceivedData
                            { receivedUserData = Just myUserData
                            , receivedLoanDetails = Nothing
                            , receivedCreditBureauData = Nothing
                            }
                         ]

      it "registers two users when two registration events are received" $ do
        runMultiple readModel [UserDataRegistered myUserData, UserDataRegistered notMyUserData]
          `shouldOutput` [ ReceivedData
                            { receivedUserData = Just myUserData
                            , receivedLoanDetails = Nothing
                            , receivedCreditBureauData = Nothing
                            }
                         , ReceivedData
                            { receivedUserData = Just notMyUserData
                            , receivedLoanDetails = Nothing
                            , receivedCreditBureauData = Nothing
                            }
                         ]

    describe "whole" $ do
      it "registers one user when a registration command is received" $ do
        run whole (RegisterUserData myUserData)
          `shouldOutput` [ ReceivedData
                            { receivedUserData = Just myUserData
                            , receivedLoanDetails = Nothing
                            , receivedCreditBureauData = Nothing
                            }
                         , ReceivedData
                            { receivedUserData = Just myUserData
                            , receivedLoanDetails = Nothing
                            , receivedCreditBureauData = Just creditBureauData
                            }
                         ]

      it "registers two users when two registration commands are received" $ do
        runMultiple whole [RegisterUserData myUserData, RegisterUserData notMyUserData]
          `shouldOutput` [ ReceivedData
                            { receivedUserData = Just myUserData
                            , receivedLoanDetails = Nothing
                            , receivedCreditBureauData = Nothing
                            }
                         , ReceivedData
                            { receivedUserData = Just myUserData
                            , receivedLoanDetails = Nothing
                            , receivedCreditBureauData = Just creditBureauData
                            }
                         , ReceivedData
                            { receivedUserData = Just notMyUserData
                            , receivedLoanDetails = Nothing
                            , receivedCreditBureauData = Just creditBureauData
                            }
                         ]

    describe "riskApplication" $ do
      it "registers one user" $ do
        run riskApplication (RegisterUserData myUserData)
          `shouldOutput` ReceivedData
            { receivedUserData = Just myUserData
            , receivedLoanDetails = Nothing
            , receivedCreditBureauData = Just creditBureauData
            }

      it "updates the user data" $ do
        runMultiple riskApplication [RegisterUserData notMyUserData, RegisterUserData myUserData]
          `shouldOutput` ReceivedData
            { receivedUserData = Just myUserData
            , receivedLoanDetails = Nothing
            , receivedCreditBureauData = Just creditBureauData
            }

      it "collects the user data and the loan details" $ do
        runMultiple riskApplication [RegisterUserData myUserData, ProvideLoanDetails loanDetails]
          `shouldOutput` ReceivedData
            { receivedUserData = Just myUserData
            , receivedLoanDetails = Just loanDetails
            , receivedCreditBureauData = Just creditBureauData
            }

      it "updates the loan details" $ do
        runMultiple
          riskApplication
          [ RegisterUserData myUserData
          , ProvideLoanDetails loanDetails
          , ProvideLoanDetails otherLoanDetails
          ]
          `shouldOutput` ReceivedData
            { receivedUserData = Just myUserData
            , receivedLoanDetails = Just otherLoanDetails
            , receivedCreditBureauData = Just creditBureauData
            }
