module Main where

import "base" System.Environment (getArgs)
import "doctest-parallel" Test.DocTest
import "doctest-parallel" Test.DocTest.Helpers

main :: IO ()
main = do
  args <- getArgs
  cremPackage <- findCabalPackage "crem"
  cremLib <- extractCabalLibrary cremPackage
  cremExamplesLib <- extractSpecificCabalLibrary (Just "crem-examples") cremPackage
  let
    base = mergeLibraries [cremLib, cremExamplesLib]
    wholeCremLib = base
      { <ghcOptionsField> = "-package-id" : "crem-examples" : <ghcOptionsField> base }