{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Integration.Regedit where

import System.Exit    (ExitCode (..))
import System.Process (readProcessWithExitCode)

import Test.HUnit (Test (TestCase), assertEqual)


runStackExec :: [String] -> IO (ExitCode, String, String)
runStackExec args = readProcessWithExitCode "stack" ("exec" : "haskell-student-directory-exe" : "--" : args ++ ["--path=./test/temp/"]) ""

-- Entity registration tests - exception handling

testAddInvalidEntity :: Test
testAddInvalidEntity = TestCase $ do
  (exitCode, _, stderr) <- runStackExec ["add", "foobar", "foobar"]
  let expectedResponse = "Unknown Entity: foobar\nOptions: (student, lecturer, module)\n"
  assertEqual "Assert program exit code" (ExitFailure 1) exitCode
  assertEqual "Assert program stderr output" expectedResponse stderr

testAddExistingStudent :: Test
testAddExistingStudent = TestCase $ do
  (exitCode, _, stderr) <- runStackExec ["add", "student", "1023476", "John", "Smith", "john.smith@winchester.ac.uk"]
  let expectedResponse = "A student with a matching identifier already exists.\nPlease enter a unique identifier.\n"
  assertEqual "Assert program exit code" (ExitFailure 1) exitCode
  assertEqual "Assert program stderr output" expectedResponse stderr

testAddExistingLecturer :: Test
testAddExistingLecturer = TestCase $ do
  (exitCode, _, stderr) <- runStackExec ["add", "lecturer", "5278913", "Dr.", "Rachel", "Lee", "rachel.lee@winchester.ac.uk"]
  let expectedResponse = "A lecturer with a matching identifier already exists.\nPlease enter a unique identifier.\n"
  assertEqual "Assert program exit code" (ExitFailure 1) exitCode
  assertEqual "Assert program stderr output" expectedResponse stderr

testAddExistingModule :: Test
testAddExistingModule = TestCase $ do
  (exitCode, _, stderr) <- runStackExec ["add", "module", "1234", "Introduction to Computer Science", "Science", "5278913"]
  let expectedResponse = "A module with a matching identifier already exists.\nPlease enter a unique identifier.\n"
  assertEqual "Assert program exit code" (ExitFailure 1) exitCode
  assertEqual "Assert program stderr output" expectedResponse stderr


-- Entity update tests - exception handling

testUpdateInvalidEntity :: Test
testUpdateInvalidEntity = TestCase $ do
  (exitCode, _, stderr) <- runStackExec ["update", "foobar", "foobar", "-", "-", "-"]
  let expectedResponse = "Unknown Entity: foobar\nOptions: (student, lecturer, module)\n"
  assertEqual "Assert program exit code" (ExitFailure 1) exitCode
  assertEqual "Assert program stderr output" expectedResponse stderr

testUpdateInvalidStudent :: Test
testUpdateInvalidStudent = TestCase $ do
  (exitCode, _, stderr) <- runStackExec ["update", "student", "foobar", "-", "-", "-"]
  let expectedResponse = "Student not found.\n"
  assertEqual "Assert program exit code" (ExitFailure 1) exitCode
  assertEqual "Assert program stderr output" expectedResponse stderr

testUpdateInvalidLecturer :: Test
testUpdateInvalidLecturer = TestCase $ do
  (exitCode, _, stderr) <- runStackExec ["update", "lecturer", "foobar", "-", "-", "-", "-"]
  let expectedResponse = "Lecturer not found.\n"
  assertEqual "Assert program exit code" (ExitFailure 1) exitCode
  assertEqual "Assert program stderr output" expectedResponse stderr

testUpdateInvalidModule :: Test
testUpdateInvalidModule = TestCase $ do
  (exitCode, _, stderr) <- runStackExec ["update", "module", "foobar", "-", "-", "-"]
  let expectedResponse = "Module not found.\n"
  assertEqual "Assert program exit code" (ExitFailure 1) exitCode
  assertEqual "Assert stderr output" expectedResponse stderr 


-- Entity deletion tests - exception handling

testDeleteInvalidEntity :: Test
testDeleteInvalidEntity = TestCase $ do
  (exitCode, _, stderr) <- runStackExec ["delete", "foobar", "foobar"]
  let expectedResponse = "Unknown Entity: foobar\nOptions: (student, lecturer, module)\n"
  assertEqual "Assert program exit code" (ExitFailure 1) exitCode
  assertEqual "Assert program stderr output" expectedResponse stderr

testDeleteInvalidStudent :: Test
testDeleteInvalidStudent = TestCase $ do
  (exitCode, _, stderr) <- runStackExec ["delete", "student", "foobar"]
  let expectedResponse = "Student not found.\n"
  assertEqual "Assert program exit code" (ExitFailure 1) exitCode
  assertEqual "Assert program stderr output" expectedResponse stderr

testDeleteInvalidLecturer :: Test
testDeleteInvalidLecturer = TestCase $ do
  (exitCode, _, stderr) <- runStackExec ["delete", "lecturer", "foobar"]
  let expectedResponse = "Lecturer not found.\n"
  assertEqual "Assert program exit code" (ExitFailure 1) exitCode
  assertEqual "Assert program stderr output" expectedResponse stderr

testDeleteInvalidModule :: Test
testDeleteInvalidModule = TestCase $ do
  (exitCode, _, stderr) <- runStackExec ["delete", "module", "foobar"]
  let expectedResponse = "Module not found.\n"
  assertEqual "Assert program exit code" (ExitFailure 1) exitCode
  assertEqual "Assert program stderr output" expectedResponse stderr


-- Lifecycle tests - verifying the functionality of add, update and delete commands

testLifecycleStudent :: Test
testLifecycleStudent = TestCase $ do

  -- Add a new student
  (exitCodeA, stdoutA, _) <- runStackExec  ["add", "student", "9999999", "Cathryn", "Skidmore", "cathryn.skidmore@winchester.ac.uk"]
  let expectedResponseA = "Student added sucessfully.\n"
  assertEqual "Assert program exit code" ExitSuccess exitCodeA
  assertEqual "Assert program stdout response" expectedResponseA stdoutA

  -- Update the student
  (exitCodeB, stdoutB, _) <- runStackExec  ["update", "student", "9999999", "-", "Harris", "cathryn.harris@winchester.ac.uk"]
  let expectedResponseB = "Student details sucessfully updated.\n"
  assertEqual "Assert program exit code" ExitSuccess exitCodeB
  assertEqual "Assert program stdout response" expectedResponseB stdoutB

  -- Assert that the student has been correctly added and updated
  (exitCodeC, stdoutC, _) <- runStackExec  ["show", "student", "9999999"]
  let expectedResponseC = [ "[Student] (Identifier: 9999999) (Name: Cathryn Harris) (Email: cathryn.harris@winchester.ac.uk)\n"
                          , "Error Loading Enrolled Modules:"
                          , "No enrolled modules."
                          ]
  assertEqual "Assert programe exit code" ExitSuccess exitCodeC
  assertEqual "Assert program stdout response" (unlines expectedResponseC) stdoutC

  -- Delete the student
  (exitCodeD, stdoutD, _) <- runStackExec  ["delete", "student", "9999999"]
  let expectedResponseD = "Student sucessfully deleted.\n"
  assertEqual "Assert program exit code" ExitSuccess exitCodeD
  assertEqual "Assert program stdout response" expectedResponseD stdoutD


testLifecycleLecturer :: Test
testLifecycleLecturer = TestCase $ do

  -- Add a new lecturer
  (exitCodeA, stdoutA, _) <- runStackExec  ["add", "lecturer", "9999999", "Professor", "Glyn", "Normington", "glyn.normington@winchester.ac.uk"]
  let expectedResponseA = "Lecturer added sucessfully.\n"
  assertEqual "Assert program exit code" ExitSuccess exitCodeA
  assertEqual "Assert program stdout response" expectedResponseA stdoutA

  -- Update the lectuer
  (exitCodeB, stdoutB, _) <- runStackExec  ["update", "lecturer", "9999999", "Dr.", "-", "-", "-"]
  let expectedResponseB = "Lecturer details sucessfully updated.\n"
  assertEqual "Assert program exit code" ExitSuccess exitCodeB
  assertEqual "Assert program stdout response" expectedResponseB stdoutB

  -- Verify the lecturer has been correctly added and updated
  (exitCodeC, stdoutC, _) <- runStackExec  ["show", "lecturer", "9999999"]
  let expectedResponseC = "[Lecturer] (Identifier: 9999999) (Name: Dr. Glyn Normington) (Email: glyn.normington@winchester.ac.uk)\n"
  assertEqual "Assert programe exit code" ExitSuccess exitCodeC
  assertEqual "Assert program stdout response" expectedResponseC stdoutC

  -- Delete the lecturer
  (exitCodeD, stdoutD, _) <- runStackExec  ["delete", "lecturer", "9999999"]
  let expectedResponseD = "Lecturer sucessfully deleted.\n"
  assertEqual "Assert program exit code" ExitSuccess exitCodeD
  assertEqual "Assert program stdout response" expectedResponseD stdoutD


testLifecycleModule :: Test
testLifecycleModule = TestCase $ do

  -- Attempt to add a new module, specifying an invalid lecturer identifier
  (exitCode, _, stderr) <- runStackExec  ["add", "module", "9999", "Functional Programming", "Science", "9999999"]
  let expectedResponseA = "The specified lecturer could not be found.\n"
  assertEqual "Assert program exit code" (ExitFailure 1) exitCode
  assertEqual "Assert program stderr output" expectedResponseA stderr

  _ <- runStackExec  ["add", "lecturer", "9999999", "Professor", "Glyn", "Normington", "glyn.normington@winchester.ac.uk"]

  -- Add a new module, specifying an valid lecturer identifier
  (exitCodeB, stdoutB, _) <- runStackExec  ["add", "module", "9999", "Functional Programming", "Science", "9999999"]
  let expectedResponseB = "Module added sucessfully.\n"
  assertEqual "Assert program exit code" ExitSuccess exitCodeB
  assertEqual "Assert program stdout response" expectedResponseB stdoutB

  -- Update the module
  (exitCodeC, stdoutC, _) <- runStackExec  ["update", "module", "9999", "Intoduction to Functional Programming", "Digital Technologies", "-"]
  let expectedResponseC = "Module details sucessfully updated.\n"
  assertEqual "Assert program exit code" ExitSuccess exitCodeC
  assertEqual "Assert program stdout response" expectedResponseC stdoutC

  -- Verify the module has been correctly added and updated
  (exitCodeD, stdoutD, _) <- runStackExec  ["show", "module", "9999"]
  let expectedResponseD = [ "[Module] (Identifier: 9999) (Name: Intoduction to Functional Programming) (Department: Digital Technologies) (Lecturer: 9999999)\n"
                          , "Error Loading Enrolled Students:"
                          , "No enrolled students."
                          ]
  assertEqual "Assert programe exit code" ExitSuccess exitCodeD
  assertEqual "Assert program stdout response" (unlines expectedResponseD) stdoutD

  _ <- runStackExec  ["delete", "lecturer", "9999999"]

  -- Delete the module
  (exitCodeE, stdoutE, _) <- runStackExec  ["delete", "module", "9999"]
  let expectedResponseE = "Module sucessfully deleted.\n"
  assertEqual "Assert program exit code" ExitSuccess exitCodeE
  assertEqual "Assert program stdout response" expectedResponseE stdoutE
