module Command.Search (runSearch) where

import Data.Char (toLower)

import Library (Error, readLecturers, readModules, readStudents)

import Text.Regex.Posix ((=~))

-- To increse search sensitity regex based matching is used

runSearch :: String -> String -> FilePath -> IO (Either Error String)
runSearch entity query path = case map toLower entity of
  "student" -> do
    readResult <- readStudents path
    case readResult of
      Left errorMsg  -> return (Left errorMsg)
      Right students -> do

        -- Use filter and regex to match students
        case filter (\s -> show s =~ (".*" ++ query ++ ".*") :: Bool) students of
          []     -> return (Left "No matching students.")
          result -> return (Right (init (concatMap (\s -> show s ++ "\n") result)))

  "lecturer" -> do
    readResult <- readLecturers path
    case readResult of
      Left errorMsg   -> return (Left errorMsg)
      Right lecturers -> do

        -- Use filter and regex to match lecturers
        case filter (\l -> show l =~ (".*" ++ query ++ ".*") :: Bool) lecturers of
          []     -> return (Left "No matching lecturers.")
          result -> return (Right (init (concatMap (\l -> show l ++ "\n") result)))

  "module" -> do
    readResult <- readModules path
    case readResult of
      Left errorMsg -> return (Left errorMsg)
      Right modules -> do

        -- Use filter and regex to match modules
        case filter (\m -> show m =~ (".*" ++ query ++ ".*") :: Bool) modules of
          []     -> return (Left "No matching modules.")
          result -> return (Right (init (concatMap (\m -> show m ++ "\n") result)))

  _ -> return (Left ("Unknown Entity: " ++ entity ++ "\nOptions: (student, lecturer, module)"))
