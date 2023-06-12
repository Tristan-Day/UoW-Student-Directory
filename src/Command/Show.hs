module Command.Show (runShow, inRelation, getStudent, getLecturer, getModule, getEnrolledModules, getEnrolledStudents) where

import Data.Char  (toLower)
import Data.Maybe (mapMaybe)

import Library
    ( Error
    , Identifier
    , Lecturer (Lecturer)
    , Module (Module)
    , Relation (Relation)
    , Student (Student)
    , findEntity
    , readEnrollment
    , readLecturers
    , readModules
    , readStudents
    )

inRelation :: Identifier -> Relation -> Bool
inRelation i (Relation domain codomain) =
  i == domain || i == codomain

getEnrolledModules :: Student -> FilePath -> IO (Either Error [Module])
getEnrolledModules (Student student _ _ _) path = do
  -- Read enrollment
  readEnrollmentResult <- readEnrollment path
  case readEnrollmentResult of
    Left errorMsg    -> return (Left errorMsg)
    Right enrollment -> do

      -- Read modules
      readModulesResult <- readModules path
      case readModulesResult of
        Left errorMsg -> return (Left errorMsg)
        Right modules -> do

          case mapMaybe (\(Relation _ identifier) -> findEntity (Module identifier "" "" "") modules) $ filter (inRelation student) enrollment of
            [] -> return (Left "No enrolled modules.")
            s  -> return (Right s)

getEnrolledStudents :: Module -> FilePath -> IO (Either Error [Student])
getEnrolledStudents (Module module' _ _ _) path = do
  -- Read enrollment
  readEnrollmentResult <- readEnrollment path
  case readEnrollmentResult of
    Left errorMsg -> return (Left errorMsg)
    Right enrollment -> do

      -- Read students
      readStudentsResult <- readStudents path
      case readStudentsResult of
        Left errorMsg  -> return (Left errorMsg)
        Right students -> do

          -- Using 'mapMaybe' to discard any Nothing values
          case mapMaybe (\(Relation identifier _) -> findEntity (Student identifier "" "" "") students) (filter (inRelation module') enrollment) of
            [] -> return (Left "No enrolled students.")
            s  -> return (Right s)

getStudent :: Identifier -> FilePath -> IO (Either Error Student)
getStudent identifier path = do
  readResult <- readStudents path
  case readResult of
    Left errorMsg -> return (Left errorMsg)
    Right students -> do
      case findEntity (Student identifier "" "" "") students of
        Nothing      -> return $ Left "Student not found."
        Just student -> return $ Right student

getLecturer :: Identifier -> FilePath -> IO (Either Error Lecturer)
getLecturer identifier path = do
  readResult <- readLecturers path
  case readResult of
    Left errorMsg -> return (Left errorMsg)
    Right lecturers -> do
      case findEntity (Lecturer identifier "" "" "" "") lecturers of
        Nothing       -> return (Left "Lecturer not found.")
        Just lecturer -> return (Right lecturer)

getModule :: Identifier -> FilePath -> IO (Either Error Module)
getModule identifier path = do
  readResult <- readModules path
  case readResult of
    Left errorMsg -> return (Left errorMsg)
    Right modules -> do
      case findEntity (Module identifier "" "" "") modules of
        Nothing      -> return (Left "Module not found.")
        Just module' -> return (Right module')

runShow :: String -> Identifier -> FilePath -> IO (Either Error String)
runShow entity identifier path = case map toLower entity of
  "student" -> do
    result <- getStudent identifier path
    case result of
      Left errorMsg -> return (Left errorMsg)
      Right student -> do

        -- Get enrolled modules
        getModulesResult <- getEnrolledModules student path
        case getModulesResult of
          Left  errorMsg -> return (Right (show student ++ "\n\nError Loading Enrolled Modules:\n" ++ errorMsg))

          -- Concatinate the list of enrolled modules, discarding the last \n with 'init'
          Right modules  -> return (Right (show student ++ "\n\nEnrolled Modules:\n\n" ++ init (concatMap (\m -> show m ++ "\n") modules)))

  "lecturer" -> do
    result <- getLecturer identifier path
    case result of
      Left errorMsg  -> return (Left errorMsg)
      Right lecturer -> return (Right (show lecturer))

  "module" -> do
    result <- getModule identifier path
    case result of
      Left errorMsg -> return (Left errorMsg)
      Right module' -> do
        -- Get enrolled students
        getModulesResult <- getEnrolledStudents module' path
        case getModulesResult of
          Left  errorMsg -> return (Right (show module' ++ "\n\nError Loading Enrolled Students:\n" ++ errorMsg))

          -- Concatinate the list of enrolled students, discarding the last \n with 'init'
          Right modules  -> return (Right (show module' ++ "\n\nEnrolled Students:\n\n" ++ init (concatMap (\m -> show m ++ "\n") modules)))

  _ -> return (Left ("Unknown Entity: " ++ entity ++ "\nOptions: (student, lecturer, module)"))
