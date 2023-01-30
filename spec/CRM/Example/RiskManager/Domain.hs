module CRM.Example.RiskManager.Domain where

import "base" Data.String (IsString)

newtype Name = Name String
  deriving newtype (Eq, Show, IsString)

newtype Surname = Surname String
  deriving newtype (Eq, Show, IsString)

newtype TaxCode = TaxCode String
  deriving newtype (Eq, Show, IsString)

data UserData = UserData
  { name :: Name
  , surname :: Surname
  , taxCode :: TaxCode
  }
  deriving stock (Eq, Show)

newtype Amount = EuroCents Int
  deriving newtype (Eq, Show)

newtype InstalmentsNumber = InstalmentsNumber Int
  deriving newtype (Eq, Show)

data LoanDetails = LoanDetails
  { amount :: Amount
  , instalments :: InstalmentsNumber
  }
  deriving stock (Eq, Show)

newtype MissedPaymentDeadlines = MissedPaymentDeadlines Int
  deriving newtype (Eq, Show)

data CreditBureauData = CreditBureauData
  { missedPaymentDeadlines :: MissedPaymentDeadlines
  , arrears :: Amount
  }
  deriving stock (Eq, Show)

-- ** State machine data types

data RiskCommand
  = RegisterUserData UserData
  | ProvideLoanDetails LoanDetails
  | ProvideCreditBureauData CreditBureauData
  deriving stock (Eq, Show)

data RiskEvent
  = UserDataRegistered UserData
  | LoanDetailsProvided LoanDetails
  | CreditBureauDataReceived CreditBureauData
  deriving stock (Eq, Show)
