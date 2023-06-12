module Command.Enrol (runEnrol) where

import Command.Show (getModule, getStudent)

import Library
    ( Error
    , Identifier
    , Relation (Relation)
    , findEntity
    , readEnrollment
    , writeEnrollment
    )

runEnrol :: Identifier -> Identifier -> FilePath -> IO (Either Error String)
runEnrol student module' path = do
  -- Check the student exists
  checkStudent <- getStudent student path
  case checkStudent of
    Left errorMsg -> return (Left errorMsg)
    Right _       -> do

      -- Check the module exists
      checkModule <- getModule module' path
      case checkModule of
        Left errorMsg -> return (Left errorMsg)
        Right _       -> do

          -- Read existing enrollment
          readResult <- readEnrollment path
          case readResult of
            Left errorMsg    -> return (Left errorMsg)
            Right enrollment -> do

              -- Check the student is not already enrolled
              case findEntity (Relation student module') enrollment of
                Just _  -> return (Left "Cannot Enroll Student:\nStudent is already enrolled.")
                Nothing -> do
                  writeEnrollment (Relation student module' : enrollment) path
                  return (Right "Student sucesssfully enrolled.")
