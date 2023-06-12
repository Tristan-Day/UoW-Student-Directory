module Command.Export (runExport) where

import Command.Show
    ( getEnrolledModules
    , getEnrolledStudents
    , getModule
    , getStudent
    )

import Data.Char        (toLower)
import Data.Time.Clock  (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)

import GHC.IO.Handle.FD   (withFile)
import GHC.IO.Handle.Text (hPutStr)
import GHC.IO.IOMode      (IOMode (WriteMode))

import Library (Error, Identifier, Module (..), Student (..))

generateMarkdownTable :: [String] -> [[String]] -> String
generateMarkdownTable fields rows =
  "| " ++ concatMap (++ " | ") fields ++     "\n" ++
  "| " ++ concatMap (const "- | ") fields ++ "\n" ++
  concatMap (\row -> "| " ++ concatMap (++ " | ") row ++ "\n") rows

generateReportFooter :: IO String
generateReportFooter = do
  time <- getCurrentTime
  return $ "---\nRendered by Haskell Student Directory - " ++ formatTime defaultTimeLocale "%Y-%m-%d" time

generateStudentReport :: Student -> FilePath -> IO (Either Error String)
generateStudentReport student path = do
  -- Get enrolled modules
  readModulesResult <- getEnrolledModules student path
  case readModulesResult of
    Left errorMsg -> return (Left $ "Failed to generate report: " ++ errorMsg)
    Right modules -> do

      -- Generate rows for the student details section
      let studentDetails = [ ["ID", studentIdentifier student]
                           , ["Name", studentFirstname student ++ " " ++ studentLastname student]
                           , ["Email", studentEmail student]
                           ]

      -- Generate rows for the enrolled modules section
      let moduleList = map (\m -> [ moduleIdentifier m
                                  , moduleName m
                                  , moduleDepartment m
                                  , moduleLecturer m
                                  ]) modules

      let title = "# Report for Student #" ++ studentIdentifier student ++ "\n<br/>\n\n"
      let body  = "## Details\n" ++ generateMarkdownTable [" ", " "] studentDetails ++ "\n<br/>\n\n" ++
                  "## Enrolled Modules\n" ++ generateMarkdownTable ["ID", "Name", "Department", "Lecurer ID"] moduleList ++ "\n<br/>\n\n"

      footer <- generateReportFooter
      return (Right $ title ++ body ++ footer)

generateModuleReport :: Module -> FilePath -> IO (Either Error String)
generateModuleReport module' path = do
  -- Get enrolled students
  readStudentsResult <- getEnrolledStudents module' path
  case readStudentsResult of
    Left errorMsg  -> return (Left $ "Failed to generate report: " ++ errorMsg)
    Right students -> do

      -- Generate rows for the module details section
      let moduleDetails = [ ["ID", moduleIdentifier module']
                          , ["Name", moduleName module']
                          , ["Department", moduleDepartment module']
                          , ["Lecturer", moduleLecturer module']
                          ]

      -- Generate rows for the enrolled students section
      let studentList = map (\s -> [ studentIdentifier s
                                    , studentFirstname s
                                    , studentLastname s
                                    , studentEmail s
                                    ]) students

      let title = "# Report for Module #" ++ moduleIdentifier module' ++ "\n<br/>\n\n"
      let body  = "## Details\n" ++ generateMarkdownTable [" ", " "] moduleDetails ++ "\n<br/>\n\n" ++
                  "## Enrolled Students\n" ++ generateMarkdownTable ["ID", "Firstname", "Lastname", "Email Address"] studentList ++ "\n<br/>\n\n"

      footer <- generateReportFooter
      return (Right $ title ++ body ++ footer)

runExport :: String -> Identifier -> FilePath -> FilePath -> IO (Either Error String)
runExport entity identifier outputPath path = case map toLower entity of
  "student" -> do
    readResult <- getStudent identifier path
    case readResult of
      Left errorMsg -> return (Left errorMsg)
      Right student -> do

          -- Generate the report
          generateReportResult <- generateStudentReport student path
          case generateReportResult of
            Left errorMsg -> return (Left errorMsg)
            Right report  -> do

              -- Export the report to a file
              withFile (outputPath ++ "Student Report - #" ++ identifier ++ ".md") WriteMode $ \handle -> do
                hPutStr handle report
              return (Right "Student report exported.")

  "module" -> do
    readResult <- getModule identifier path
    case readResult of
      Left errorMsg -> return (Left errorMsg)
      Right module' -> do

          -- Generate a module report
          generateReportResult <- generateModuleReport module' path
          case generateReportResult of
            Left errorMsg -> return (Left errorMsg)
            Right report  -> do

              -- Export the report to a file
              withFile (outputPath ++ "Module Report - #" ++ identifier ++ ".md") WriteMode $ \handle -> do
                hPutStr handle report
              return (Right "Module report exported.")

  _ -> return (Left ("Unknown Entity: " ++ entity ++ "\nOptions: (student, module)"))

