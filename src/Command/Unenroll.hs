module Command.Unenroll (runUnenroll) where

import Command.Show (runShow)

import Library
    ( Error
    , Identifier
    , Relation (Relation)
    , deleteEntity
    , findEntity
    , readEnrollment
    , writeEnrollment
    )

runUnenroll :: Identifier -> Identifier -> FilePath -> IO (Either Error String)
runUnenroll student module' path = do

    -- Check the student exists
  checkStudent <- runShow "student" student path
  case checkStudent of
    Left errorMsg -> return (Left errorMsg)
    Right _       -> do

      -- Check the module exists
      checkModule <- runShow "module" module' path
      case checkModule of
        Left errorMsg -> return (Left errorMsg)
        Right _       -> do

          -- Read existing enrollment
          readResult <- readEnrollment path
          case readResult of
            Left errorMsg    -> return (Left errorMsg)
            Right enrollment -> do

              -- Check student is enrolled
              case findEntity (Relation student module') enrollment of
                Nothing -> return (Left "Cannot Unenroll Student:\nStudent is not enrolled.")
                Just _  -> do
                  writeEnrollment (deleteEntity (Relation student module') enrollment) path
                  return (Right "Student sucessfully unenrolled.")

