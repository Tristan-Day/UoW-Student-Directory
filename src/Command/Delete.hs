module Command.Delete (runDelete) where

import Data.Char (toLower)

import Library
    ( Error
    , Identifier
    , Lecturer (Lecturer)
    , Module (Module)
    , Student (Student)
    , deleteEntity
    , findEntity
    , readLecturers
    , readModules
    , readStudents
    , writeLecturers
    , writeModules
    , writeStudents
    )

runDelete :: String -> Identifier -> FilePath -> IO (Either Error String)
runDelete entity identifier path = case map toLower entity of
  "student" -> do
    readResult <- readStudents path
    case readResult of
      Left errorMsg  -> return (Left $ "Failed to load student data: " ++ errorMsg)
      Right students -> do

        -- Check the student exists
        case findEntity (Student identifier "" "" "") students of
          Nothing      -> return (Left "Student not found.")

          -- Remove the student
          Just student -> do
            writeStudents (deleteEntity student students) path
            return (Right "Student sucessfully deleted.")

  "lecturer" -> do
    readResult <- readLecturers path
    case readResult of
      Left errorMsg   -> return (Left $ "Failed to load lecurer data: " ++ errorMsg)
      Right lecturers -> do

        -- Check if the lecturer exists
        case findEntity (Lecturer identifier "" "" "" "") lecturers of
          Nothing       -> return (Left "Lecturer not found.")

          -- Remove the lecturer
          Just lecturer -> do
            writeLecturers (deleteEntity lecturer lecturers) path
            return (Right "Lecturer sucessfully deleted.")

  "module" -> do
    readResult <- readModules path
    case readResult of
      Left errorMsg -> return (Left $ "Failed to load module data: " ++ errorMsg)
      Right modules -> do

        -- Check if the module exists
        case findEntity (Module identifier "" "" "") modules of
          Nothing      -> return (Left "Module not found.")

          -- Remove the module
          Just module' -> do
            writeModules (deleteEntity module' modules) path
            return (Right "Module sucessfully deleted.")

  _ -> return (Left ("Unknown Entity: " ++ entity ++ "\nOptions: (student, lecturer, module)"))
