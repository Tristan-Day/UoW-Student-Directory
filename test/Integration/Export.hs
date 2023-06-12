{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Integration.Export where

import System.Directory (doesFileExist)
import System.Exit      (ExitCode (..))
import System.Process   (readProcessWithExitCode)

import Test.HUnit (Test (TestCase), assertBool, assertEqual)


runStackExec :: [String] -> IO (ExitCode, String, String)
runStackExec args = readProcessWithExitCode "stack" ("exec" : "haskell-student-directory-exe" : "--" : args ++ ["--path=./test/temp/"]) ""

testExportInvalidStudent :: Test
testExportInvalidStudent = TestCase $ do
  (exitCode, _, stderr) <- runStackExec ["export", "student", "foobar"]
  let expectedResponse = "Student not found.\n"
  assertEqual "Assert program exit code" (ExitFailure 1) exitCode
  assertEqual "Assert program stderr output" expectedResponse stderr

testExportInvalidModule :: Test
testExportInvalidModule = TestCase $ do
  (exitCode, _, stderr) <- runStackExec ["export", "module", "foobar"]
  let expectedResponse = "Module not found.\n"
  assertEqual "Assert program exit code" (ExitFailure 1) exitCode
  assertEqual "Assert program stderr output" expectedResponse stderr

testExportValidStudent :: Test
testExportValidStudent = TestCase $ do
  (exitCode, stdout, _) <- runStackExec  ["export", "student", "6480125", "--output=./test/temp/"]

  let expectedResponse = "Student report exported.\n"
  assertEqual "Assert program exit code" ExitSuccess exitCode
  assertEqual "Assert program stdout out" expectedResponse stdout

  outputExists <- doesFileExist "./test/temp/Student Report - #6480125.md"
  assertBool "Assert report file exists" outputExists

testExportValidModule :: Test
testExportValidModule = TestCase $ do
  (exitCode, stdout, _) <- runStackExec  ["export", "module", "5678", "--output=./test/temp/"]

  let expectedResponse = "Module report exported.\n"
  assertEqual "Assert program exit code" ExitSuccess exitCode
  assertEqual "Assert program stdout response" expectedResponse stdout

  outputExists <- doesFileExist "./test/temp/Module Report - #5678.md"
  assertBool "Assert report file exists" outputExists
