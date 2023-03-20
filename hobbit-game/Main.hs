{-# LANGUAGE DataKinds #-}

module Main where

import "base" Data.Functor.Identity
import "base" System.IO.Error
import "crem" Crem.Example.TheHobbit
import "crem" Crem.StateMachine

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
    commandLoop :: IO HobbitCommand
    commandLoop = catchIOError readLn . const $ do
      putStrLn "The command you provided is invalid. The valid commands are GoEast, GoWest, GoNorth, GoSouth, Wait, GetKey and UnlockDoor. Please try again."
      commandLoop

    loop :: StateMachineT Identity HobbitCommand HobbitMessage -> IO ()
    loop machine = do
      command <- commandLoop
      let
        (message, machine') = runIdentity $ run machine command
      putStrLn $ getMessage message
      loop machine'
