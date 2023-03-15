{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Crem.DeciderSpec where

import Crem.StateMachine (StateMachineT (..), run)
import "base" Data.Functor.Identity (Identity)
import "crem" Crem.BaseMachine (InitialState (..))
import "crem" Crem.Decider
import "crem" Crem.Example.Uno
import "hspec" Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "Decider" $ do
    describe "unoDecider" $ do
      it "is able to rebuild from a list of events" $ do
        let
          unoInitialDecider :: Decider UnoTopology Command (Either GameError Event)
          unoInitialDecider = unoDecider (InitialState UnoInitialState)

          rebuiltDecider :: Decider UnoTopology Command (Either GameError Event)
          rebuiltDecider =
            rebuildDecider
              [ Right $
                  GameStarted
                    (InitialData (PlayerCount 3) (DigitCard Three Yellow))
                    (PlayerId 0)
              , Right $
                  CardPlayed
                    (PlayData (PlayerId 0) (DigitCard Six Yellow))
              ]
              unoInitialDecider

          inProgressDecider :: Decider UnoTopology Command (Either GameError Event)
          inProgressDecider =
            unoDecider
              ( InitialState $
                  UnoStartedState $
                    StateData
                      { topCard = DigitCard Six Yellow
                      , currentPlayer = Player (PlayerId 0) (PlayerCount 3) Clockwise
                      }
              )

          command :: Command
          command =
            PlayCard $
              PlayData
                { playerId = PlayerId 1
                , card = DigitCard Six Blue
                }
         in
          (fst <$> run @Identity (Basic $ deciderMachine rebuiltDecider) command)
            `shouldBe` (fst <$> run (Basic $ deciderMachine inProgressDecider) command)
