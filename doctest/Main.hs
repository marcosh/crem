module Main where

import "base" System.Environment (getArgs)
import "doctest-parallel" Test.DocTest (mainFromCabal)

main :: IO ()
main = do
  mainFromCabal "crem" =<< getArgs
