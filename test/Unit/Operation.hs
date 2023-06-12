{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Unit.Operation where

import Command.Show (inRelation)

import Library
    ( Relation (Relation)
    , Student (Student)
    , deleteEntity
    , findEntity
    )

import Test.HUnit (Test (TestCase), assertBool, assertEqual)


-- Relation tests

testInRelationInvalid :: Test
testInRelationInvalid = TestCase $ do
  let target = Relation "7889654" "1234"
  assertBool "Test valid domain" $ not (inRelation "FooBar" target)

testInRelationValidDomain :: Test
testInRelationValidDomain = TestCase $ do
  let target = Relation "7889654" "1234"
  assertBool "Test valid domain" (inRelation "7889654" target)

testInRelationValidCodomain :: Test
testInRelationValidCodomain = TestCase $ do
  let target = Relation "7889654" "1234"
  assertBool "Test valid domain" (inRelation "1234" target)


-- Entity location tests

testFindEntityEmpty :: Test
testFindEntityEmpty = TestCase $ do
  let target = Student "7889654" "Thomas" "Robinson" "thomas.robinson@winchester.ac.uk"
  assertEqual "Assert empty input result" Nothing (findEntity target [])

testFindEntityInvalid :: Test
testFindEntityInvalid = TestCase $ do
  let entities = [ Student "1234567" "James"    "McDonald" "james.mcdonald@winchester.ac.uk"
                 , Student "2345678" "Emma"     "Smith"    "emma.smith@winchester.ac.uk"
                 , Student "9012345" "Ethan"    "Thompson" "ethan.thompson@winchester.ac.uk"
                 , Student "0123456" "Isabella" "Garcia"   "isabella.garcia@winchester.ac.uk"
                 ]
  let target   = Student "7889654" "Thomas" "Robinson" "thomas.robinson@winchester.ac.uk"
  assertEqual "Assert invalid target result" Nothing (findEntity target entities)

testFindEntityValid :: Test
testFindEntityValid = TestCase $ do
  let entities = [ Student "1234567" "James"    "McDonald" "james.mcdonald@winchester.ac.uk"
                 , Student "2345678" "Emma"     "Smith"    "emma.smith@winchester.ac.uk"
                 , Student "9012345" "Ethan"    "Thompson" "ethan.thompson@winchester.ac.uk"
                 , Student "0123456" "Isabella" "Garcia"   "isabella.garcia@winchester.ac.uk"
                 ]
  let target   = Student "7889654" "Thomas" "Robinson" "thomas.robinson@winchester.ac.uk"
  assertEqual "Assert invalid target result" (Just target) (findEntity target (target : entities))


-- Entity deletion tests

testDeleteEntityEmpty :: Test
testDeleteEntityEmpty = TestCase $ do
  let target = Student "7889654" "Thomas" "Robinson" "thomas.robinson@winchester.ac.uk"
  assertEqual "Assert empty input result" [] (deleteEntity target [])

testDeleteEntityInvalid :: Test
testDeleteEntityInvalid = TestCase $ do
  let entities = [ Student "1234567" "James"    "McDonald" "james.mcdonald@winchester.ac.uk"
                 , Student "2345678" "Emma"     "Smith"    "emma.smith@winchester.ac.uk"
                 , Student "9012345" "Ethan"    "Thompson" "ethan.thompson@winchester.ac.uk"
                 , Student "0123456" "Isabella" "Garcia"   "isabella.garcia@winchester.ac.uk"
                 ]
  let target   = Student "7889654" "Thomas" "Robinson" "thomas.robinson@winchester.ac.uk"
  assertEqual "Assert invalid target result" entities (deleteEntity target entities)

testDeleteEntityValid :: Test
testDeleteEntityValid = TestCase $ do
  let entities = [ Student "1234567" "James"    "McDonald" "james.mcdonald@winchester.ac.uk"
                 , Student "2345678" "Emma"     "Smith"    "emma.smith@winchester.ac.uk"
                 , Student "9012345" "Ethan"    "Thompson" "ethan.thompson@winchester.ac.uk"
                 , Student "0123456" "Isabella" "Garcia"   "isabella.garcia@winchester.ac.uk"
                 ]
  let target   = Student "7889654" "Thomas" "Robinson" "thomas.robinson@winchester.ac.uk"
  assertEqual "Assert sucessful deletion result" entities (deleteEntity target (target : entities))
