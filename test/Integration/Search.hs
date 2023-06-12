{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Integration.Search where

import System.Exit    (ExitCode (..))
import System.Process (readProcessWithExitCode)

import Test.HUnit (Test (TestCase), assertEqual)


runStackExec :: [String] -> IO (ExitCode, String, String)
runStackExec args = readProcessWithExitCode "stack" ("exec" : "haskell-student-directory-exe" : "--" : args ++ ["--path=./test/temp/"]) ""

testSearchInvalidEntity :: Test
testSearchInvalidEntity = TestCase $ do
  (exitCode, _, stderr) <- runStackExec ["search", "foobar", "foobar"]
  let expectedResponse = "Unknown Entity: foobar\nOptions: (student, lecturer, module)\n"
  assertEqual "Assert program exit code" (ExitFailure 1) exitCode
  assertEqual "Assert program stderr output" expectedResponse stderr

-- Tests for searching students

testSearchNoMatchesStudent :: Test
testSearchNoMatchesStudent = TestCase $ do
  (exitCode, _, stderr) <- runStackExec ["search", "student", "foobar"]
  let expectedResponse = "No matching students.\n"
  assertEqual "Assert program exit code" (ExitFailure 1) exitCode
  assertEqual "Assert program stderr output" expectedResponse stderr

testSearchMatchingStudent :: Test
testSearchMatchingStudent = TestCase $ do
  (exitCode, stdout, _) <- runStackExec ["search", "student", "John"]
  let expectedResponse = [ "[Student] (Identifier: 1023476) (Name: John Smith) (Email: john.smith@winchester.ac.uk)"
                         , "[Student] (Identifier: 7142095) (Name: Michael Johnson) (Email: michael.johnson@winchester.ac.uk)"
                         ]
  assertEqual "Assert program exit code" ExitSuccess exitCode
  assertEqual "Assert program stdout response" (unlines expectedResponse) stdout


-- Tests for searching lecturers

testSearchNoMatchesLecturer :: Test
testSearchNoMatchesLecturer = TestCase $ do
  (exitCode, _, stderr) <- runStackExec ["search", "lecturer", "foobar"]
  let expectedResponse = "No matching lecturers.\n"
  assertEqual "Assert program exit code" (ExitFailure 1) exitCode
  assertEqual "Assert program stderr output" expectedResponse stderr

testSearchMatchingLecturer :: Test
testSearchMatchingLecturer = TestCase $ do
  (exitCode, stdout, _) <- runStackExec ["search", "lecturer", "Lee"]
  let expectedResponse = [ "[Lecturer] (Identifier: 5278913) (Name: Dr. Rachel Lee) (Email: rachel.lee@winchester.ac.uk)"
                         , "[Lecturer] (Identifier: 5278999) (Name: Dr. Kevin Lee) (Email: kevin.lee@winchester.ac.uk)"
                         ]
  assertEqual "Assert program exit code" ExitSuccess exitCode
  assertEqual "Assert program stdout response" (unlines expectedResponse) stdout


-- Tests for searching modules

testSearchNoMatchesModule :: Test
testSearchNoMatchesModule = TestCase $ do
  (exitCode, _, stderr) <- runStackExec ["search", "module", "foobar"]
  let expectedResponse = "No matching modules.\n"
  assertEqual "Assert program exit code" (ExitFailure 1) exitCode
  assertEqual "Assert program stderr output" expectedResponse stderr

testSearchMatchingModule :: Test
testSearchMatchingModule = TestCase $ do
  (exitCode, stdout, _) <- runStackExec ["search", "module", "Computer"]
  let expectedResponse = [ "[Module] (Identifier: 1234) (Name: Introduction to Computer Science) (Department: Science) (Lecturer: 5278913)"
                         , "[Module] (Identifier: 6789) (Name: Computer Networks) (Department: Science) (Lecturer: 2051689)"
                         ]
  assertEqual "Assert program exit code" ExitSuccess exitCode
  assertEqual "Assert program stdout response" (unlines expectedResponse) stdout
