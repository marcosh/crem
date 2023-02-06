{-# LANGUAGE DataKinds #-}

module Main where

import "crem" Crem.Example.TheHobbit
import "crem" Crem.StateMachine
import "base" Data.Functor.Identity

main :: IO ()
main = do
  putStrLn "Welcome to the Hobbit adventure game!"
  let
    initialState :: HobbitState 'TunnelLikeHall
    initialState = TunnelLikeHallState

    initialMessage :: String
    initialMessage = getMessage $ stateMessage initialState
  putStrLn initialMessage
  loop (Basic $ hobbitMachine initialState)
  where
    loop :: StateMachineT Identity HobbitCommand HobbitMessage -> IO ()
    loop machine = do
      command <- readLn
      let
        (message, machine') = runIdentity $ run machine command
      putStrLn $ getMessage message
      loop machine'
