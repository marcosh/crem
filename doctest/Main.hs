{-# LANGUAGE ImplicitParams #-}

module Main where

import "base" Control.Monad (unless)
import "base" Data.List (isPrefixOf, isInfixOf)
import "base" System.Exit (exitFailure)
import "directory" System.Directory (getCurrentDirectory, listDirectory)
import "filepath" System.FilePath ((</>))
import "doctest-parallel" Test.DocTest (isSuccess, setSeed)
import "doctest-parallel" Test.DocTest.Helpers
import "doctest-parallel" Test.DocTest.Internal.Logging (LogLevel (..))
import "doctest-parallel" Test.DocTest.Internal.Options (defaultModuleConfig)
import "doctest-parallel" Test.DocTest.Internal.Runner (runModules)

main :: IO ()
main = do
  cremPackage <- findCabalPackage "crem"
  cremLib <- extractCabalLibrary cremPackage
  cremExamplesLib <- extractSpecificCabalLibrary (Just "crem-examples") cremPackage
  let wholeCremLib = mergeLibraries [cremLib, cremExamplesLib]
  pkgId <- findCremExamplesId
  let
    (includeArgs, allModules, otherGhciArgs) = libraryToGhciArgs wholeCremLib
    evalGhciArgs = otherGhciArgs ++ ["-package-id", pkgId]
    parseGhcArgs = includeArgs ++ otherGhciArgs
  modConfig <- let ?verbosity = Info in setSeed defaultModuleConfig
  summary <- let ?verbosity = Info in runModules modConfig Nothing True parseGhcArgs evalGhciArgs allModules
  unless (isSuccess summary) exitFailure

findCremExamplesId :: IO String
findCremExamplesId = do
  dir <- getCurrentDirectory
  files <- listDirectory dir
  let envFiles = filter (isPrefixOf ".ghc.environment.") files
  case envFiles of
    [] -> error "No .ghc.environment file found; run cabal build first"
    (f:_) -> do
      contents <- readFile (dir </> f)
      let pkgIdLines = filter (isPrefixOf "package-id") (lines contents)
          examplesLines = filter (isInfixOf "crem-examples") pkgIdLines
      case examplesLines of
        [] -> error "Could not find crem-examples package-id in .ghc.environment file"
        (l:_) -> pure (drop (length "package-id ") l)