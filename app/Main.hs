module Main (main) where

import           System.Exit         (ExitCode (..), exitWith)
import           System.IO           (hPutStrLn, stderr)

import           Options.Applicative as Opt

import           Command.Add         (runAdd)
import           Command.Update      (runUpdate)
import           Command.Delete      (runDelete)
import           Command.Enrol       (runEnrol)
import           Command.Export      (runExport)
import           Command.Initialise  (runInitalise)
import           Command.Search      (runSearch)
import           Command.Show        (runShow)
import           Command.Unenroll    (runUnenroll)

type Entity = String
type Identifier = String

data Command =
  Show         Entity     Identifier
  | Search     Entity     String
  | Add        Entity     [String]
  | Update     Entity     Identifier [String]
  | Delete     Entity     Identifier
  | Enrol      Identifier Identifier
  | Unenroll   Identifier Identifier
  | Export     Entity     Identifier FilePath
  | Initialise Bool

parseShow :: Parser Command
parseShow =
  Show
    <$> argument str (metavar "ENTITY" <> help "The type of entity to reterive")
    <*> argument str (metavar "IDENTIFIER" <> help "The identifier of the entity to reterive")

parseSearch :: Parser Command
parseSearch =
  Search
    <$> argument str (metavar "ENTITY" <> help "The type of entity to reterive")
    <*> argument str (metavar "QUERY" <> help "The string to match to")

parseAdd :: Parser Command
parseAdd =
  Add
    <$> argument str (metavar "ENTITY" <> help "The type of entity to reterive")
    <*> many         (strArgument (metavar "FIELDS" <> help "The field values to add"))

parseUpdate :: Parser Command
parseUpdate = 
  Update
    <$> argument str (metavar "ENTITY" <> help "The type of entity to update")
    <*> argument str (metavar "IDENTIFIER" <> help "The identifier of the entity to update")
    <*> many         (strArgument (metavar "FIELDS" <> help "The new field values (Use - to skip)"))

parseDelete :: Parser Command
parseDelete =
  Delete
    <$> argument str (metavar "ENTITY" <> help "The type of entity to reterive")
    <*> argument str (metavar "IDENTIFIER" <> help "The identifier of the entity to delete")

parseEnrol :: Parser Command
parseEnrol =
  Enrol
    <$> argument str (metavar "STUDENT" <> help "The identifer of the student to enrol")
    <*> argument str (metavar "MODULE" <> help "The identifer of the module to enrol the target in")

parseUnenroll :: Parser Command
parseUnenroll =
  Unenroll
    <$> argument str (metavar "STUDENT" <> help "The identifer of the student to unenrol")
    <*> argument str (metavar "MODULE" <> help "The identifer of the module to unenroll the target from")

parseExport :: Parser Command
parseExport =
  Export
    <$> argument str (metavar "ENTITY" <> help "The type of entity to reterive")
    <*> argument str (metavar "IDENTIFIER" <> help "The identifier of the entity to export")
    <*> strOption    (metavar "FILENAME" <> long "output" <> short 'o' <> help "The name of the output file to export to" <> value "")

parseInitialise :: Parser Command
parseInitialise =
  Initialise <$> switch (long "overwrite" <> short 'f' <> help "Overwrites any existing files")

parseCommand :: Parser Command
parseCommand =
  subparser $
    mconcat
      [ Opt.command "show"       (info parseShow       (progDesc "Shows the properties of a single entity from a given identifier"))
      , Opt.command "search"     (info parseSearch     (progDesc "Returns multiple entites matching a given string"))
      , Opt.command "add"        (info parseAdd        (progDesc "Adds a new entity to the directory"))
      , Opt.command "update"     (info parseUpdate     (progDesc "Updates an existing entity in the directory"))
      , Opt.command "delete"     (info parseDelete     (progDesc "Removes an entity from the directory"))
      , Opt.command "enrol"      (info parseEnrol      (progDesc "Enrols a given student in a specified module"))
      , Opt.command "unenroll"   (info parseUnenroll   (progDesc "Unenrolls a given student in a specified module"))
      , Opt.command "export"     (info parseExport     (progDesc "Exports details of a given entity to an external file"))
      , Opt.command "initialise" (info parseInitialise (progDesc "Initialise a new student directory"))
      ]

data Options = Options {command :: Command, path :: FilePath}

parseOptions :: Parser Options
parseOptions = do
  Options
    <$> parseCommand
    <*> strOption (long "path" <> metavar "PATH" <> help "Directory containing the data files" <> value "")

main :: IO ()
main = do
  options <- execParser args
  result <- runCommand options
  case result of
    Left errorMsg -> do
      hPutStrLn stderr errorMsg
      exitWith (ExitFailure 1)
    Right resultMsg ->
      putStrLn resultMsg
  where
    args = info (parseOptions <**> helper) (fullDesc <> header "A Haskell-based Student Administration Program")

runCommand :: Options -> IO (Either String String)
runCommand options =
  case Main.command options of
    Show entity identifier ->
      fmap (either Left Right) (runShow entity identifier (path options))

    Search entity query ->
      fmap (either Left Right) (runSearch entity query (path options))

    Add entity fields ->
      fmap (either Left Right) (runAdd entity fields (path options))

    Update entity identifier fields ->
      fmap (either Left Right) (runUpdate entity identifier fields (path options))

    Delete entity identifier ->
      fmap (either Left Right) (runDelete entity identifier (path options))

    Enrol student module' ->
      fmap (either Left Right) (runEnrol student module' (path options))

    Unenroll student module' ->
      fmap (either Left Right) (runUnenroll student module' (path options))

    Initialise overwrite ->
      fmap (either Left Right) (runInitalise overwrite (path options))
    
    Export entity identifier outputPath ->
      fmap (either Left Right) (runExport entity identifier outputPath (path options))
