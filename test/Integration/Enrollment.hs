{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Integration.Enrollment where

import System.Exit    (ExitCode (..))
import System.Process (readProcessWithExitCode)

import Test.HUnit (Test (TestCase), assertEqual)

runStackExec :: [String] -> IO (ExitCode, String, String)
runStackExec args = readProcessWithExitCode "stack" ("exec" : "haskell-student-directory-exe" : "--" : args ++ ["--path=./test/temp/"]) ""

testEnrolInvalidStudent :: Test
testEnrolInvalidStudent = TestCase $ do
  (exitCode, _, stderr) <- runStackExec ["enrol", "foobar", "1234"]
  let expectedResponse = "Student not found.\n"
  assertEqual "Assert program exit code" (ExitFailure 1) exitCode
  assertEqual "Assert program stderr output" expectedResponse stderr

testEnrolInvalidModule :: Test
testEnrolInvalidModule = TestCase $ do
  (exitCode, _, stderr) <- runStackExec ["enrol", "1023476", "foobar"]
  let expectedResponse = "Module not found.\n"
  assertEqual "Assert program exit code" (ExitFailure 1) exitCode
  assertEqual "Assert program stderr output" expectedResponse stderr

testUnenrollInvalidStudent :: Test
testUnenrollInvalidStudent = TestCase $ do
  (exitCode, _, stderr) <- runStackExec ["unenroll", "foobar", "1234"]
  let expectedResponse = "Student not found.\n"
  assertEqual "Assert program exit code" (ExitFailure 1) exitCode
  assertEqual "Assert program stderr output" expectedResponse stderr

testUnenrollInvalidModule :: Test
testUnenrollInvalidModule = TestCase $ do
  (exitCode, _, stderr) <- runStackExec ["unenroll", "1023476", "foobar"]
  let expectedResponse = "Module not found.\n"
  assertEqual "Assert program exit code" (ExitFailure 1) exitCode
  assertEqual "Assert program stderr output" expectedResponse stderr

testEnrolValid :: Test
testEnrolValid = TestCase $ do
  (exitCodeA, stdoutA, _) <- runStackExec  ["enrol", "1023476", "1234"]
  let expectedResponseA = "Student sucesssfully enrolled.\n"
  assertEqual "Assert program exit code" ExitSuccess exitCodeA
  assertEqual "Assert program stdout response" expectedResponseA stdoutA

  (exitCodeB, stdoutB, _) <- runStackExec ["unenroll", "1023476", "1234"]
  let expectedResponseB = "Student sucessfully unenrolled.\n"
  assertEqual "Assert program exit code" ExitSuccess exitCodeB
  assertEqual "Assert program stdout response" expectedResponseB stdoutB
