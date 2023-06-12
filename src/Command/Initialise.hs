module Command.Initialise (runInitalise) where

import Library
    ( Error
    , writeEnrollment
    , writeLecturers
    , writeModules
    , writeStudents
    )

import System.Directory (doesFileExist)

runInitalise :: Bool -> FilePath -> IO (Either Error String)
runInitalise overwrite path = do
  if overwrite then do
    initialise path
    return (Right "Directory successfully initialised.")
  else do
    existingFiles <- mapM (\f -> doesFileExist $ path ++ f) ["students.csv", "lecturers.csv", "modules.csv", "enrollment.csv"]
    if or existingFiles then
      return (Left "Directory already initialised. Use --overwrite to clear data.")
    else do
      initialise path
      return (Right "Directory successfully initialised.")

initialise :: FilePath -> IO ()
initialise path = do
  writeStudents   [] path
  writeLecturers  [] path
  writeModules    [] path
  writeEnrollment [] path
