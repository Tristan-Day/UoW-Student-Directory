{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Integration.Initialise where

import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Exit      (ExitCode (..))
import System.Process   (readProcessWithExitCode)

import Test.HUnit (Test (TestCase), assertBool, assertEqual)


runStackExec :: [String] -> IO (ExitCode, String, String)
runStackExec args = readProcessWithExitCode "stack" ("exec" : "haskell-student-directory-exe" : "--" : args ++ ["--path=./test/temp/"]) ""

testInitialiseSafety :: Test
testInitialiseSafety = TestCase $ do
  (exitCode, _, stderr) <- runStackExec ["initialise"]
  let expectedResponse = "Directory already initialised. Use --overwrite to clear data.\n"
  assertEqual "Assert program exit code" (ExitFailure 1) exitCode
  assertEqual "Assert program stderr output" expectedResponse stderr

testInitialise :: Test
testInitialise = TestCase $ do
  let testDirectory = "./test/temp/initialise/"
  createDirectoryIfMissing True testDirectory

  (exitCodeA, stdoutA, _) <- readProcessWithExitCode "stack" ["exec", "haskell-student-directory-exe", "--", "initialise", "--path=" ++ testDirectory] ""
  let expectedResponseA = "Directory successfully initialised.\n"
  assertEqual "Assert program exit code" ExitSuccess exitCodeA
  assertEqual "Assert program stdout response" expectedResponseA stdoutA

  fileCheckA <- doesFileExist $ testDirectory ++ "students.csv"
  assertBool "Assert students.csv exists" fileCheckA

  fileCheckB <- doesFileExist $ testDirectory ++ "lecturers.csv"
  assertBool "Assert lecturers.csv exists" fileCheckB

  fileCheckC <- doesFileExist $ testDirectory ++ "modules.csv"
  assertBool "Assert modules.csv exists" fileCheckC

  fileCheckD <- doesFileExist $ testDirectory ++ "enrollment.csv"
  assertBool "Assert enrollment.csv exists" fileCheckD

  -- Check the directory was sucessfully initalised
  (exitCodeB, _, _) <- readProcessWithExitCode "stack" ["exec", "haskell-student-directory-exe", "--", "add", "lecturer", "9999999", "Professor", "Glyn", "Normington", "glyn.normington@winchester.ac.uk", "--path=" ++ testDirectory] ""
  assertEqual "Assert creation of operable directory" ExitSuccess exitCodeB

  (exitCodeC, stdoutC, _) <- readProcessWithExitCode "stack" ["exec", "haskell-student-directory-exe", "--", "initialise", "--overwrite", "--path=" ++ testDirectory] ""
  let expectedResponseC = "Directory successfully initialised.\n"
  assertEqual "Assert program exit code" ExitSuccess exitCodeC
  assertEqual "Assert program stdout response" expectedResponseC stdoutC

  -- Check the directory was sucessfully reset
  (exitCodeD, _, _) <- readProcessWithExitCode "stack" ["exec", "haskell-student-directory-exe", "--", "show", "lecturer", "9999999", "--path=" ++ testDirectory] ""
  assertEqual "Assert directory was reset" (ExitFailure 1) exitCodeD
