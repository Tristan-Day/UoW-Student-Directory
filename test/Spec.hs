import Integration.Enrollment
import Integration.Export
import Integration.Initialise
import Integration.Regedit
import Integration.Search
import Integration.Show

import System.Directory

import Test.HUnit

import Unit.Entity
import Unit.Operation

main :: IO ()
main = do
    putStrLn "------- Preparing Test Environment -------"

    let testDirectory = "./test/temp/"

    contents <- listDirectory "./test/data"
    createDirectoryIfMissing True testDirectory
    mapM_ (\i -> copyFile ("./test/data/" ++ i) (testDirectory ++ i)) contents

    putStrLn "\nTest Environment Prepared\n"

    putStrLn "----------- Running Unit Tests -----------"

    putStrLn "\nTesting Student Entity Parser"
    _ <- runTestTT   (test [testStudentParseEmpty,  testStudentParseValid])

    putStrLn "\nTesting Lecturer Entity Parser"
    _ <- runTestTT   (test [testLecturerParseEmpty, testLecturerParseValid])

    putStrLn "\nTesting Module Entity Parser"
    _ <- runTestTT   (test [testModuleParseEmpty,   testModuleParseValid])

    putStrLn "\nTesting Command Operations"
    _ <- runTestTT   (test [ testFindEntityEmpty
                           , testFindEntityInvalid
                           , testFindEntityValid
                           , testDeleteEntityEmpty
                           , testDeleteEntityInvalid
                           , testDeleteEntityValid
                           , testInRelationInvalid
                           , testInRelationValidDomain
                           , testInRelationValidCodomain
                           ])

    putStrLn "\n------- Running Integration Tests -------"

    putStrLn "\nTesting Show Command"
    _ <- runTestTT (test [ Integration.Show.testShowInvalidEntity
                         , Integration.Show.testShowInvalidStudent
                         , Integration.Show.testShowValidStudent
                         , Integration.Show.testShowInvalidLecturer
                         , Integration.Show.testShowValidLecturer
                         , Integration.Show.testShowInvalidModule
                         , Integration.Show.testShowValidModule
                         ])

    putStrLn "\nTesting Search Command"
    _ <- runTestTT (test [ Integration.Search.testSearchInvalidEntity
                         , Integration.Search.testSearchNoMatchesStudent
                         , Integration.Search.testSearchNoMatchesLecturer
                         , Integration.Search.testSearchNoMatchesModule
                         , Integration.Search.testSearchMatchingStudent
                         , Integration.Search.testSearchMatchingLecturer
                         , Integration.Search.testSearchMatchingModule
                         ])

    putStrLn "\nTesting Enrol and Unenroll Commands"
    _ <- runTestTT (test [ Integration.Enrollment.testEnrolInvalidStudent
                         , Integration.Enrollment.testEnrolInvalidModule
                         , Integration.Enrollment.testUnenrollInvalidStudent
                         , Integration.Enrollment.testUnenrollInvalidModule
                         , Integration.Enrollment.testEnrolValid
                         ])

    putStrLn "\nTesting Add, Update and Delete Commands"
    _ <- runTestTT (test [ Integration.Regedit.testAddInvalidEntity
                         , Integration.Regedit.testAddExistingStudent
                         , Integration.Regedit.testAddExistingModule
                         , Integration.Regedit.testAddExistingLecturer
                         , Integration.Regedit.testDeleteInvalidEntity
                         , Integration.Regedit.testDeleteInvalidStudent
                         , Integration.Regedit.testDeleteInvalidLecturer
                         , Integration.Regedit.testDeleteInvalidModule
                         , Integration.Regedit.testUpdateInvalidEntity
                         , Integration.Regedit.testUpdateInvalidStudent
                         , Integration.Regedit.testUpdateInvalidLecturer
                         , Integration.Regedit.testUpdateInvalidModule
                         , Integration.Regedit.testLifecycleStudent
                         , Integration.Regedit.testLifecycleLecturer
                         , Integration.Regedit.testLifecycleModule
                         ])

    putStrLn "\nTesting Export Command"
    _ <- runTestTT (test [ Integration.Export.testExportInvalidStudent
                         , Integration.Export.testExportInvalidModule
                         , Integration.Export.testExportValidStudent
                         , Integration.Export.testExportValidModule
                         ])

    putStrLn "\nTesting Initialise Command"
    _ <- runTestTT (test [ Integration.Initialise.testInitialiseSafety
                         , Integration.Initialise.testInitialise
                         ])

    putStrLn "\n-------------- Cleaning Up --------------"

    removeDirectoryRecursive testDirectory
    putStrLn "\nFinished Cleaning Up"
