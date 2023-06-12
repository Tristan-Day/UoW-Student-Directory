{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Unit.Entity where

import Library

import Test.HUnit (Test (TestCase), assertBool, assertEqual, assertFailure)


-- Tests for the student parser

testStudentParseEmpty :: Test
testStudentParseEmpty = TestCase $
  case result of
    Left errorMsg -> assertBool    "Parse empty student data" (expected == errorMsg)
    Right _       -> assertFailure "Unexpected result"
  where
    expected = "Student record must contain exactly four fields: identifier, firstname, lastname and email address"
    result = parseStudent []

testStudentParseValid :: Test
testStudentParseValid = TestCase $
  case result of
    Left errorMsg -> assertFailure ("Unexpected result: " ++ errorMsg)
    Right student -> do
      assertEqual "Identifier"    "1023476"                     (studentIdentifier student)
      assertEqual "First Name"    "John"                        (studentFirstname  student)
      assertEqual "Last Name"     "Smith"                       (studentLastname   student)
      assertEqual "Email Address" "john.smith@winchester.ac.uk" (studentEmail      student)
  where
    result = parseStudent ["1023476", "John", "Smith", "john.smith@winchester.ac.uk"]


-- Tests for the lecturer parser

testLecturerParseEmpty :: Test
testLecturerParseEmpty = TestCase $
  case result of
    Left errorMsg -> assertBool    "Parse empty lecturer data" (expected == errorMsg)
    Right _       -> assertFailure "Unexpected result"
  where
    expected = "Lecturer record must contain exactly five fields: identifier, title, firstname, lastname and email address"
    result = parseLecturer []

testLecturerParseValid :: Test
testLecturerParseValid = TestCase $
  case result of
    Left errorMsg  -> assertFailure ("Unexpected result: " ++ errorMsg)
    Right lecturer -> do
      assertEqual "Identifier"    "5278913"                     (lecturerIdentifier lecturer)
      assertEqual "Title"         "Dr."                         (lectuerTitle       lecturer)
      assertEqual "First Name"    "Rachel"                      (lectuerFirstname   lecturer)
      assertEqual "Last Name"     "Lee"                         (lectuerLastname    lecturer)
      assertEqual "Email Address" "rachel.lee@winchester.ac.uk" (lecturerEmail      lecturer)
  where
    result = parseLecturer ["5278913", "Dr.", "Rachel", "Lee", "rachel.lee@winchester.ac.uk"]


-- Tests for the module parser

testModuleParseEmpty :: Test
testModuleParseEmpty = TestCase $
  case result of
    Left errorMsg -> assertBool    "Parse empty module data" (expected == errorMsg)
    Right _       -> assertFailure "Unexpected result"
  where
    expected = "Module record must exactly four fields: identifier, name, department and lecturer identifier"
    result = parseModule []

testModuleParseValid :: Test
testModuleParseValid = TestCase $
  case result of
    Left errorMsg -> assertFailure ("Unexpected result: " ++ errorMsg)
    Right module' -> do
      assertEqual "Identifier"    "1234"                             (moduleIdentifier module')
      assertEqual "Name"          "Introduction to Computer Science" (moduleName       module')
      assertEqual "Department"    "Science"                          (moduleDepartment module')
      assertEqual "Lecturer"      "5278913"                          (moduleLecturer   module')
  where
    result = parseModule ["1234", "Introduction to Computer Science", "Science", "5278913"]
