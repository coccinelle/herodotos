module SQL
  ( versions
  , bugs
  , getTables
  )where

import Bugs
import Psql
import Utils

import Data.List
import Control.Monad.List

import Text.Parsec
import Text.Parsec.String

---------------------
--                 --
--    REQUESTS     --
--                 --
---------------------


{- SQL To create the view :

select version_name , files.file_id, standardized_name as standardized_report_name
  from correlations , error_names, files, reports
  where     correlations.report_error_name = error_names.report_error_name
        and correlations.correlation_id    = reports.correlation_id
        and files.file_id                  = reports.file_id
;

-}

-- SQL Functions

-- Versions
sql_versions         = "SELECT DISTINCT version_name FROM version_file_report ORDER BY version_name"

versions :: ([String] -> [String]) -> PsqlM [String]
versions f = do s <- psqlCmd sql_versions
                return $ sortBy versionOrdering $ f $ readVersions s

versionOrdering :: String -> String -> Ordering
versionOrdering s1 s2 = compare (v2li s1) (v2li s2)
  where
    readi :: String -> Integer
    readi = read

    v2li s = map readi $ tail $ sepPred (\c -> c == '-' || c == '.') s


-- Lines of Files Per Bugs

sql_files_per_bug vs tp =
   "SELECT number, COUNT(*) as files FROM (\
\    SELECT file_id , COUNT(*) AS number FROM version_file_report\
\    WHERE version_name='" ++ vs ++ "' "
     ++ (whichBugs tp) ++ " GROUP BY file_id)\
\  TableReponse GROUP BY number ORDER BY number"


files_per_bug :: String -> BugSelect -> PsqlM (String, [(Integer, Integer)])
files_per_bug v bs = do let sql = sql_files_per_bug v bs
	      	     	let _  = print sql
	      	     	s <- psqlCmd sql
                        return $ (sql, readBugLine s)


-- Bugs

vrs2where [] = ""
vrs2where  l = "WHERE " ++ (concat $ intersperse " or " $ map aux l)
  where
    aux k = "version_name=\'" ++ k ++ "\'"


sql_bugs vrs = "SELECT DISTINCT standardized_report_name FROM version_file_report "
               ++ (vrs2where vrs)
               ++ " ORDER BY standardized_report_name"

bugs :: ([String] -> [String]) -> PsqlM [String]
bugs f = do psqlDebugLn $ "=> Gettings versions"
            vrs <- versions f
            psqlDebugLn $ "=> Sending to psql: " ++ (sql_bugs vrs)
            s <- psqlCmd $ sql_bugs vrs
            return $ readBugs s


-- Bug Selection


whichBugs :: BugSelect -> String
whichBugs (BugSelect i l) =
     if null l
      then ""
      else " and " ++  (foldr2 (print_op i) "" (map (print_type i) l))
  where
    print_op With    s1 s2 = s1 ++ " or "  ++ s2
    print_op Without s1 s2 = s1 ++ " and " ++ s2

    print_type With    s = "standardized_report_name='"  ++ s ++ "'"
    print_type Without s = "standardized_report_name!='" ++ s ++ "'"

bugSelectName :: BugSelect -> String
bugSelectName (BugSelect _ []) = ""
bugSelectName (BugSelect i l ) = (f1 i) ++ (foldr f2 "" l)
  where
    f1 With    = "_with"
    f1 Without = "_without"

    f2 x y = "_" ++ x ++ y



------------------------
--                    --
--     RESPONSE       --
--                    --
------------------------


-- Parser

int :: Parser Integer
int = do s <- many1 digit
         return $ (read s :: Integer)

isVersionLine :: String -> Either ParseError String
isVersionLine = parse p ""
  where
    p = do spaces
           string "linux"
           version <- many1 $ noneOf ['\'',' ' , '\t' , '\n' , '|' ]
           spaces
           eof
           return ("linux" ++ version)


isBugLine :: String -> Either ParseError (Integer, Integer)
isBugLine = parse p ""
  where
    p = do spaces
           b <- int
           spaces
           oneOf ['|']
           spaces
           f <- int
           eof
           return (b,f)


isBug :: String -> Either ParseError String
isBug = parse p ""
  where
   p = do space
          str <- many1 (noneOf [' ', '\t' , '\n'])
          spaces
          eof
          return str

readVersions :: [String] -> [String]
readVersions str = mapFilterEither isVersionLine str

readBugLine :: [String] -> [(Integer, Integer)]
readBugLine str = mapFilterEither isBugLine str

readBugs :: [String] -> [String]
readBugs str = mapFilterEither isBug str


getTables :: BugSelect -> ([String] -> [String]) -> PsqlM [Entry]
getTables bs f = runListT aux
  where
    aux = do v <- ListT $ versions f
             (sql, l) <- lift $ files_per_bug v bs
             let vbs = v ++ (bugSelectName bs)
             return $ Entry vbs sql (normalizeName vbs) l
