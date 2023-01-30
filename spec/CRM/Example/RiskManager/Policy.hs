module CRM.Example.RiskManager.Policy where

import CRM.Example.RiskManager.Domain
import "crm" CRM.StateMachine

riskPolicy :: StateMachine RiskEvent (Maybe RiskCommand)
riskPolicy =
  stateless
    ( \case
        UserDataRegistered ud -> ProvideCreditBureauData <$> creditBureauDataForUser ud
        LoanDetailsProvided _ -> Nothing
        CreditBureauDataReceived _ -> Nothing
    )
  where
    creditBureauDataForUser :: UserData -> Maybe CreditBureauData
    creditBureauDataForUser (UserData name _ _) =
      if name == "Marco"
        then
          Just $
            CreditBureauData
              (MissedPaymentDeadlines 2)
              (EuroCents 100000)
        else Nothing
