{-# LANGUAGE FlexibleContexts #-}
module Main where

import Config
import Psql
import WXMaxima
import Bugs
import SQL
import Utils
import Options

import Control.Monad.Error
import System.Environment
import System.IO

import Text.Regex

main :: IO ()
main = runErrorT mainT >> return ()



mainT :: ErrorT String IO ()
mainT = catchError (do args <- liftIO $ getArgs
                       env  <- compileArgs args
                       let runPsqlM'      = runPsqlM $ " " ++ (psql env) ++ " -d " ++ (database env) ++ " "
                           filter_version = filter (mkPred $ kernel env)
                       (myput,myclose) <- liftIO $ mkOut (output env)
                       act  <- extractAction env
                       liftIO $ do case act of
                                     HelpMsg    -> putStrLn help_msg
                                     VersionMsg -> putStrLn version_msg
                                     Versions   -> (runPsqlM' $ versions filter_version ) >>= mapM_ myput
                                     Bugs       -> (runPsqlM' $ bugs     filter_version ) >>= mapM_ myput
                                     Tables     -> do entries <- runPsqlM' $ getTables (bs env) filter_version
                                                      output_tables (output env) (libdir env) entries >>= myput
                                   myclose
                   )
          (\e -> liftIO $ putStrLn $ "Error : " ++ e ++ "\n\n" ++ help_msg)



extractAction :: (MonadError [Char] m) => HTLEnv -> m Action
extractAction env = case action env of
                      Nothing -> throwError "No action specified"
                      Just a  -> return a

mkOut s = if s == "-"
            then let myput   = putStrLn
                     myclose = return ()
                 in  return (myput, myclose)
            else do h <- openFile (s ++ ".wxm") WriteMode
                    let myput   = hPutStrLn h
                        myclose = hClose h
                    return (myput , myclose)

mkPred ms = case ms of
               Nothing -> const True
               Just  s -> isJust . (matchRegex $ mkRegex s)

{-
                              ("versions" : _) ->
                              ("bugs"     : _) -> (runPqlM params bugs    ) >>= mapM_ putStrLn
                              ("with"     : l) -> print_tables params (BugSelect With    l)
                              ("without"  : l) -> print_tables params (BugSelect Without l)
                              _                -> print_usage
-}
