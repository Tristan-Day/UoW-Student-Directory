{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Library where

import Text.CSV (parseCSVFromFile, printCSV)

type Error = String

readCSV :: FilePath -> IO (Either Error [[String]])
readCSV path = do
  result <- parseCSVFromFile path
  case result of
    Left errorMsg -> return (Left $ "Error Reading File: " ++ show errorMsg)
    Right csvData -> return (Right $ drop 1 csvData)

writeCSV :: FilePath -> [[String]] -> IO ()
writeCSV path rows = do
  writeFile path $ printCSV rows

type Identifier = String

data Student = Student {
  studentIdentifier :: Identifier,
  studentFirstname  :: String,
  studentLastname   :: String,
  studentEmail      :: String
}

instance Show Student where
  show (Student identifier firstname lastname email) =
    "[Student] "    ++
    "(Identifier: " ++ identifier ++ ") "             ++
    "(Name: "       ++ firstname  ++ " " ++ lastname ++ ") " ++
    "(Email: "      ++ email      ++ ")"

instance Eq Student where
  (Student a _ _ _) == (Student b _ _ _) =
    a == b

data Lecturer = Lecturer {
  lecturerIdentifier :: Identifier,
  lectuerTitle       :: String,
  lectuerFirstname   :: String,
  lectuerLastname    :: String,
  lecturerEmail      :: String
}

instance Show Lecturer where
  show (Lecturer identifier title firstname lastname email) =
    "[Lecturer] "   ++
    "(Identifier: " ++ identifier ++ ") " ++
    "(Name: "       ++ title      ++ " "  ++ firstname ++ " " ++ lastname ++ ") " ++
    "(Email: "      ++ email      ++ ")"

instance Eq Lecturer where
  (Lecturer a _ _ _ _) == (Lecturer b _ _ _ _) =
    a == b

data Module = Module {
  moduleIdentifier :: Identifier,
  moduleName       :: String,
  moduleDepartment :: String,
  moduleLecturer   :: Identifier
}

instance Show Module where
  show (Module identifier name department lecturer) =
    "[Module] "     ++
    "(Identifier: " ++ identifier ++ ") " ++
    "(Name: "       ++ name       ++ ") " ++
    "(Department: " ++ department ++ ") " ++
    "(Lecturer: "   ++ lecturer   ++ ")"

instance Eq Module where
  (Module a _ _ _) == (Module b _ _ _) =
    a == b

data Relation = Relation {
  domain   :: Identifier,
  codomain :: Identifier
}

instance Eq Relation where
  (Relation a c) == (Relation b d) =
    a == b && c == d

findEntity :: Eq e => e -> [e] -> Maybe e
findEntity _ []     = Nothing
findEntity e [x]    = if e == x then Just x else Nothing
findEntity e (x:xs) = if e == x then Just x else findEntity e xs

deleteEntity :: Eq e => e -> [e] -> [e]
deleteEntity _ []     = []
deleteEntity e [x]    = [x | e /= x]
deleteEntity e (x:xs) = if e == x then xs else x : deleteEntity e xs

-- findEntities :: Eq e => [e] -> [e] -> [e]
-- findEntities [] _     = []
-- findEntities (e:xs) x =
--   case findEntity e x of
--     Nothing     -> findEntities xs x
--     Just entity -> entity : findEntities xs x

parseStudent :: [String] -> Either Error Student
parseStudent fields
  | length fields /= 4 = Left "Student record must contain exactly four fields: identifier, firstname, lastname and email address"
  | otherwise          = let [identifier, firstname, lastname, email] = fields
                         in Right (Student identifier firstname lastname email)

parseLecturer :: [String] -> Either Error Lecturer
parseLecturer fields
  | length fields /= 5 = Left "Lecturer record must contain exactly five fields: identifier, title, firstname, lastname and email address"
  | otherwise          = let [identifer, title, firstname, lastname, email] = fields
                         in Right (Lecturer identifer title firstname lastname email)

parseModule :: [String] -> Either Error Module
parseModule fields
  | length fields /= 4 = Left "Module record must exactly four fields: identifier, name, department and lecturer identifier"
  | otherwise          = let [identifier, name, department, lecturer] = fields
                         in Right (Module identifier name department lecturer)

parseRelation :: [String] -> Either Error Relation
parseRelation fields
  | length fields /= 2 = Left "Relation must contain exactly two fields"
  | otherwise          = let [domain, codomain] = fields
                         in Right (Relation domain codomain)

readStudents :: FilePath -> IO (Either Error [Student])
readStudents path = do
  csvData <- readCSV (path ++ "students.csv")
  case csvData of
    Left errorMsg -> return $ Left errorMsg
    Right rows    -> return $ mapM parseStudent rows

readLecturers :: FilePath -> IO (Either Error [Lecturer])
readLecturers path = do
  csvData <- readCSV (path ++ "lecturers.csv")
  case csvData of
    Left errorMsg -> return $ Left errorMsg
    Right rows    -> return $ mapM parseLecturer rows

readModules :: FilePath -> IO (Either Error [Module])
readModules path = do
  csvData <- readCSV (path ++ "modules.csv")
  case csvData of
    Left errorMsg -> return $ Left errorMsg
    Right rows    -> return $ mapM parseModule rows

readEnrollment :: FilePath -> IO (Either Error [Relation])
readEnrollment path = do
  csvData <- readCSV (path ++ "enrollment.csv")
  case csvData of
    Left errorMsg -> return $ Left errorMsg
    Right rows    -> return $ mapM parseRelation rows

writeStudents :: [Student] -> FilePath -> IO ()
writeStudents students path = do
  let fields = ["identifier", "firstname", "lastname", "email"]
  let rows = map (\s -> [studentIdentifier s, studentFirstname s, studentLastname s, studentEmail s]) students
  writeCSV (path ++ "students.csv") (fields : rows)

writeLecturers :: [Lecturer] -> FilePath -> IO ()
writeLecturers lecturers path = do
  let fields = ["identifier", "title", "firstname", "lastname", "email"]
  let rows = map (\l -> [lecturerIdentifier l, lectuerTitle l, lectuerFirstname l, lectuerLastname l, lecturerEmail l]) lecturers
  writeCSV (path ++ "lecturers.csv") (fields : rows)

writeModules :: [Module] -> FilePath -> IO ()
writeModules modules path = do
  let fields = ["identifer", "name", "department", "lecturer"]
  let rows = map (\m -> [moduleIdentifier m, moduleName m, moduleDepartment m, moduleLecturer m]) modules
  writeCSV (path ++ "modules.csv") (fields : rows)

writeEnrollment :: [Relation] -> FilePath -> IO ()
writeEnrollment relations path = do
  let fields = ["student", "module"]
  let rows = map (\r -> [domain r, codomain r]) relations
  writeCSV (path ++ "enrollment.csv") (fields : rows)
