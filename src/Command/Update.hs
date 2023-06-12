module Command.Update (runUpdate) where

import Command.Show (getLecturer)

import Data.Char (toLower)

import Library
    ( Error
    , Identifier
    , Lecturer (Lecturer)
    , Module (Module, moduleLecturer)
    , Student (Student)
    , deleteEntity
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

updateStudent :: Student -> Student -> Student
updateStudent (Student identifier a1 a2 a3) (Student _ b1 b2 b3) =
  Student
    identifier
    (if b1 == "-" then a1 else b1)
    (if b2 == "-" then a2 else b2)
    (if b3 == "-" then a3 else b3)

updateLecturer :: Lecturer -> Lecturer -> Lecturer
updateLecturer (Lecturer identifier a1 a2 a3 a4) (Lecturer _ b1 b2 b3 b4) =
  Lecturer
    identifier
    (if b1 == "-" then a1 else b1)
    (if b2 == "-" then a2 else b2)
    (if b3 == "-" then a3 else b3)
    (if b4 == "-" then a4 else b4)

updateModule :: Module -> Module -> Module
updateModule (Module identifier a1 a2 a3) (Module _ b1 b2 b3) =
  Module
    identifier
    (if b1 == "-" then a1 else b1)
    (if b2 == "-" then a2 else b2)
    (if b3 == "-" then a3 else b3)

runUpdate :: String -> Identifier -> [String] -> FilePath -> IO (Either Error String)
runUpdate entity identifier fields path = case map toLower entity of
  "student" -> do
    readResult <- readStudents path
    case readResult of
      Left errorMsg  -> return (Left $ "Failed to load student data: " ++ errorMsg)
      Right students -> do

        case parseStudent (identifier : fields) of
          Left errorMsg -> return (Left errorMsg)
          Right student -> do

            -- Locate the target student
            case findEntity (Student identifier "" "" "") students of
              Nothing     -> return (Left "Student not found.")
              Just target -> do

                -- Update the record and directory
                writeStudents (updateStudent target student : deleteEntity target students) path
                return (Right "Student details sucessfully updated.")

  "lecturer" -> do
    readResult <- readLecturers path
    case readResult of
      Left errorMsg   -> return (Left $ "Failed to load lecturer data: " ++ errorMsg)
      Right lecturers -> do

        case parseLecturer (identifier : fields) of
          Left errorMsg  -> return (Left errorMsg)
          Right lecturer -> do

            case findEntity (Lecturer identifier "" "" "" "") lecturers of
              Nothing     -> return (Left "Lecturer not found.")
              Just target -> do

                -- Update the record and directory
                writeLecturers (updateLecturer target lecturer : deleteEntity target lecturers) path
                return (Right "Lecturer details sucessfully updated.")

  "module" -> do
    readResult <- readModules path
    case readResult of
      Left errorMsg -> return (Left $ "Failed to load module data: " ++ errorMsg)
      Right modules -> do

        case parseModule (identifier : fields) of
          Left errorMsg -> return (Left errorMsg)
          Right module' -> do

            -- Locate the target lecturer
            case findEntity (Module identifier "" "" "") modules of
              Nothing     -> return (Left "Module not found.")
              Just target -> do

                if fields !! 2 /= "-" then do
                  -- Check the specified lecturer exists
                  checkLecturerResult <- getLecturer (moduleLecturer module') path
                  case checkLecturerResult of
                    Left _  -> return (Left "The specified lecturer could not be found. Please provide a valid lecturer identifier")
                    Right _ -> do
                      writeModules (updateModule target module' : deleteEntity target modules) path
                      return (Right "Module details sucessfully updated.")

                else do
                  -- Update the record and directory
                  writeModules (updateModule target module' : deleteEntity target modules) path
                  return (Right "Module details sucessfully updated.")

  _ -> return (Left ("Unknown Entity: " ++ entity ++ "\nOptions: (student, lecturer, module)"))
