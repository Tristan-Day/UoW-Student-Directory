{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Integration.Show where

import System.Exit    (ExitCode (..))
import System.Process (readProcessWithExitCode)

import Test.HUnit (Test (TestCase), assertEqual)


runStackExec :: [String] -> IO (ExitCode, String, String)
runStackExec args = readProcessWithExitCode "stack" ("exec" : "haskell-student-directory-exe" : "--" : args ++ ["--path=./test/temp/"]) ""

testShowInvalidEntity :: Test
testShowInvalidEntity = TestCase $ do
  (exitCode, _, stderr) <- runStackExec ["show", "foobar", "foobar"]
  let expectedResponse = "Unknown Entity: foobar\nOptions: (student, lecturer, module)\n"
  assertEqual "Assert program exit code" (ExitFailure 1) exitCode
  assertEqual "Assert program stderr output" expectedResponse stderr


-- Tests for showing students

testShowInvalidStudent :: Test
testShowInvalidStudent = TestCase $ do
  (exitCode, _, stderr) <- runStackExec ["show", "student", "foobar"]
  let expectedResponse = "Student not found.\n"
  assertEqual "Assert program exit code" (ExitFailure 1) exitCode
  assertEqual "Assert program stderr output" expectedResponse stderr

testShowValidStudent :: Test
testShowValidStudent = TestCase $ do
  (exitCodeA, stdoutA, _) <- runStackExec  ["show", "student", "1023476"]
  let expectedResponseA = "[Student] (Identifier: 1023476) (Name: John Smith) (Email: john.smith@winchester.ac.uk)\n\nError Loading Enrolled Modules:\nNo enrolled modules.\n"
  assertEqual "Assert programe exit code" ExitSuccess exitCodeA
  assertEqual "Assert program stdout response" expectedResponseA stdoutA

  (exitCodeB, stdoutB, _) <- runStackExec  ["show", "student", "6480125"]
  let expectedResponseB = [ "[Student] (Identifier: 6480125) (Name: Robert Garcia) (Email: robert.garcia@winchester.ac.uk)\n"
                          , "Enrolled Modules:\n"
                          , "[Module] (Identifier: 7890) (Name: Introduction to History) (Department: Humanities) (Lecturer: 8643921)"
                          , "[Module] (Identifier: 6789) (Name: Computer Networks) (Department: Science) (Lecturer: 2051689)"
                          , "[Module] (Identifier: 5678) (Name: Introduction to Psychology) (Department: Social Sciences) (Lecturer: 9310572)"
                          ]
  assertEqual "Assert programe exit code" ExitSuccess exitCodeB
  assertEqual "Assert program stdout response" (unlines expectedResponseB) stdoutB


-- Tests for showing lecturers

testShowInvalidLecturer :: Test
testShowInvalidLecturer = TestCase $ do
  (exitCode, _, stderr) <- runStackExec ["show", "lecturer", "foobar"]
  let expectedResponse = "Lecturer not found.\n"
  assertEqual "Assert program exit code" (ExitFailure 1) exitCode
  assertEqual "Assert program stderr output" expectedResponse stderr

testShowValidLecturer :: Test
testShowValidLecturer = TestCase $ do
  (exitCodeA, stdoutA, _) <- runStackExec  ["show", "lecturer", "5278913"]
  let expectedResponseA = "[Lecturer] (Identifier: 5278913) (Name: Dr. Rachel Lee) (Email: rachel.lee@winchester.ac.uk)\n"
  assertEqual "Assert program exit code" ExitSuccess exitCodeA
  assertEqual "Assert program stdout response" expectedResponseA stdoutA

  (exitCodeB, stdoutB, _) <- runStackExec  ["show", "lecturer", "2051689"]
  let expectedResponseB = "[Lecturer] (Identifier: 2051689) (Name: Professor David Chen) (Email: david.chen@winchester.ac.uk)\n"
  assertEqual "Assert program exit code" ExitSuccess exitCodeB
  assertEqual "Assert program stdout response" expectedResponseB stdoutB


-- Tests for showing modules

testShowInvalidModule :: Test
testShowInvalidModule = TestCase $ do
  (exitCode, _, stderr) <- runStackExec ["show", "module", "foobar"]
  let expectedResponse = "Module not found.\n"
  assertEqual "Assert program exit code" (ExitFailure 1) exitCode
  assertEqual "Assert program stderr output" expectedResponse stderr

testShowValidModule :: Test
testShowValidModule = TestCase $ do
  (exitCodeA, stdoutA, _) <- runStackExec  ["show", "module", "1234"]
  let expectedResponseA = "[Module] (Identifier: 1234) (Name: Introduction to Computer Science) (Department: Science) (Lecturer: 5278913)\n\nError Loading Enrolled Students:\nNo enrolled students.\n"
  assertEqual "Assert program exit code" ExitSuccess exitCodeA
  assertEqual "Assert program stdout response" expectedResponseA stdoutA

  (exitCodeB, stdoutB, _) <- runStackExec  ["show", "module", "7890"]
  let expectedResponseB = [ "[Module] (Identifier: 7890) (Name: Introduction to History) (Department: Humanities) (Lecturer: 8643921)\n"
                          , "Enrolled Students:\n"
                          , "[Student] (Identifier: 6480125) (Name: Robert Garcia) (Email: robert.garcia@winchester.ac.uk)"
                          , "[Student] (Identifier: 2096358) (Name: Katherine Brown) (Email: katherine.brown@winchester.ac.uk)"
                          , "[Student] (Identifier: 9258641) (Name: Jane Doe) (Email: jane.doe@winchester.ac.uk)"
                          ]
  assertEqual "Assert program exit code" ExitSuccess exitCodeB
  assertEqual "Assert program stdout response" (unlines expectedResponseB) stdoutB
