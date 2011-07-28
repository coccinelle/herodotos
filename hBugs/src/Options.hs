{-# LANGUAGE FlexibleContexts #-}
module Options where

import Bugs
import Config
import Utils

import Control.Monad.Error
import System.Console.GetOpt


-- OPTIONS

usage = "Usage: hBugs [OPTIONS] [versions|bugs|tables]"

data Action = HelpMsg | VersionMsg | Versions | Bugs | Tables
  deriving (Eq,Ord,Show,Read)

help_msg = usageInfo usage options

data HLTops = Psql         String
            | Database     String
            | BugSelectOpt BugSelect
            | LibDir       FilePath
            | Output       FilePath
            | Kernel       String
            | Help
            | Version
  deriving (Show,Eq,Ord,Read)

options =
  [ Option ['p'] ["psql"]    (ReqArg Psql         "STRING"       ) "Parameters to pass to psql"
  , Option ['d'] ["db"]      (ReqArg Database     "STRING"       ) "Database to use"
  , Option ['a'] ["with"]    (ReqArg (bs With)    "BUG1,BUG2,...") "Take only these bugs"
  , Option ['s'] ["without"] (ReqArg (bs Without) "BUG1,BUG2,...") "Take everything but these bugs"
  , Option ['l'] ["libdir"]  (ReqArg LibDir       "FILEPATH"     ) "Maxima library directory"
  , Option ['o'] ["output"]  (ReqArg Output       "FILEPATH"     ) "Output file WITHOUT the .wxm extension"
  , Option ['h'] ["help"]    (NoArg  Help                        ) "Show this help message"
  , Option ['k'] ["kernel"]  (ReqArg Kernel       "REGEX"        ) "Kernel version to use"
  , Option ['v'] ["version"] (NoArg  Version                     ) "Show version message"
  ]
  where
    bs w = BugSelectOpt . (BugSelect w) . (sepPred (== ','))


data HTLEnv = HTLEnv { psql     :: String ,
                       database :: String ,
                       bs       :: BugSelect,
                       libdir   :: Maybe FilePath,
                       output   :: FilePath,
                       kernel   :: Maybe String,
                       action   :: Maybe Action

                     }
  deriving (Eq,Show,Read,Ord)

defaultOps :: HTLEnv
defaultOps = HTLEnv ""                      -- psql
                    dft_database            -- database
                    (BugSelect Without [])  -- bs
                    Nothing                 -- libdir
                    "-"                     -- output
                    Nothing                 -- kernel
                    Nothing                 -- action


modifyOps :: HTLEnv -> HLTops -> HTLEnv
modifyOps o (Psql         s) = o { psql     = s               }
modifyOps o (Database     s) = o { database = s               }
modifyOps o (BugSelectOpt b) = o { bs       = b               }
modifyOps o (LibDir       f) = o { libdir   = Just f          }
modifyOps o (Output       f) = o { output   = f               }
modifyOps o (Kernel       k) = o { kernel   = Just k          }
modifyOps o (Help          ) = o { action   = Just HelpMsg    }
modifyOps o (Version       ) = o { action   = Just VersionMsg }



compileOps :: [HLTops] -> HTLEnv
compileOps = foldl modifyOps defaultOps

modifyNonOps :: (MonadError [Char] m) => HTLEnv -> [Char] -> m HTLEnv

modifyNonOps o s = case s of
                    "tables"   -> check Tables
                    "bugs"     -> check Bugs
                    "versions" -> check Versions
                    _          -> throwError "Unknown action"
  where
    check a = case action o of
                Nothing -> return $ o { action = Just a }
                Just _  -> throwError "Too many actions specified"

compileNonOps :: (MonadError [Char] m) => HTLEnv -> [[Char]] -> m HTLEnv
compileNonOps o = foldM modifyNonOps o


compileArgs :: (MonadError String m) => [String] -> m HTLEnv
compileArgs args = if null errors
                    then compileNonOps (compileOps ops) nonops
                    else throwError $ concat errors
  where
    (ops,nonops,errors) = getOpt Permute options args
