{-# LANGUAGE DataKinds #-}

module Main where

import "crem" Crem.BaseMachine
import "crem" Crem.Example.TheHobbit
import "crem" Crem.Render.Render
import "base" Data.Functor.Identity
import "text" Data.Text.IO qualified as Text

main :: IO ()
main = do
  let
    initialState :: HobbitState 'TunnelLikeHall
    initialState = TunnelLikeHallState

    machine :: BaseMachineT Identity HobbitTopology HobbitCommand HobbitMessage
    machine = hobbitMachine initialState

    (Mermaid mermaid) = renderStateDiagram . baseMachineAsGraph $ machine
  Text.putStrLn mermaid
