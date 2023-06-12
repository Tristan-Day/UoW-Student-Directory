module Command.Add (runAdd) where

import Command.Show (getLecturer)

import Data.Char (toLower)

import Library
    ( Error
    , Module (moduleLecturer)
    , findEntity
    , parseLecturer
    , parseModule
    , parseStudent
    , readLecturers
    , readModules
    , readStudents
    , writeLecturers
    , writeModules
    , writeStudents
    )

runAdd :: String -> [String] -> FilePath -> IO (Either Error String)
runAdd entity fields path = case map toLower entity of
  "student" -> do
    readResult <- readStudents path
    case readResult of
      Left errorMsg  -> return (Left $ "Failed to load student data: " ++ errorMsg)
      Right students -> do

        -- Parse the arguments into a student type
        case parseStudent fields of
          Left errorMsg -> return (Left errorMsg)
          Right student -> do

            -- Check for an existing student with a matching identifier
            case findEntity student students of
              Just _  -> return (Left "A student with a matching identifier already exists.\nPlease enter a unique identifier.")
              Nothing -> do

                -- Write the updated list of students to the directory
                writeStudents (student : students) path
                return (Right "Student added sucessfully.")

  "lecturer" -> do
    readResult <- readLecturers path
    case readResult of
      Left errorMsg   -> return (Left $ "Failed to load lecurer data: " ++ errorMsg)
      Right lecturers -> do

        -- Parse the arugments into a lecturer type
        case parseLecturer fields of
          Left errorMsg  -> return (Left errorMsg)
          Right lecturer -> do

            -- Check for an existing lecturer with a matching identifier
            case findEntity lecturer lecturers of
              Just _  -> return (Left "A lecturer with a matching identifier already exists.\nPlease enter a unique identifier.")
              Nothing -> do

                -- Write the updated list of lecturers to the directory
                writeLecturers (lecturer : lecturers) path
                return (Right "Lecturer added sucessfully.")

  "module" -> do
    readResult <- readModules path
    case readResult of
      Left errorMsg -> return (Left $ "Failed to load module data: " ++ errorMsg)
      Right modules -> do

        -- Parse the arguments into a module type
        case parseModule fields of
          Left errorMsg -> return (Left errorMsg)
          Right module' -> do

            -- Check for an existing module with a matching identifier
            case findEntity module' modules of
              Just _ -> return (Left "A module with a matching identifier already exists.\nPlease enter a unique identifier.")
              Nothing -> do

                -- Check that the specifed lecturer exists
                checkLecturerResult <- getLecturer (moduleLecturer module') path
                case checkLecturerResult of
                  Left _  -> return (Left "The specified lecturer could not be found.")
                  Right _ -> do

                    -- Write the updated list of modules to the directory
                    writeModules (module' : modules) path
                    return (Right "Module added sucessfully.")

  _ -> return (Left ("Unknown Entity: " ++ entity ++ "\nOptions: (student, lecturer, module)"))
