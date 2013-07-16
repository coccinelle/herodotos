module Psql where

import Config
import Utils

import Control.Monad
import Control.Monad.State
import System.Process
import System.IO



-- psql command monad interface

newtype PsqlM a = PsqlM { unPsqlM :: StateT (Handle,Handle) IO a }


instance Monad PsqlM where
  return = PsqlM . return
  (PsqlM m) >>= f = PsqlM $ m >>= (unPsqlM . f)


psqlDebug   v = PsqlM . lift . debug v
psqlDebugLn v = PsqlM . lift . debugLn v


psqlCmd :: Bool -> String -> PsqlM [String]
psqlCmd v cmd = PsqlM $ do (hinput,houtput) <- get
                           liftIO $ do debugLn v $ "=> Putting : " ++ (cmd ++ ";")
                                       hPutStr hinput (cmd ++ "\n")
                                       debugLn v $ "=> Flushing"
                                       hFlush  hinput
                                       s <- hReadFull v houtput
                                       debugLn v $ "=> Got: "
                                       mapM_ (debugLn v) s
                                       return s





runPsqlM :: String -> PsqlM a -> IO a
runPsqlM params (PsqlM m) = do (a,_) <- runStateT pm (undefined, undefined)
                               return a
  where
    pm = do (hinput,houtput,herror,hprocess) <- liftIO $ runInteractiveCommand $
                                                     invokepsql
                                                  ++ " "
                                                  ++ params
                                                  -- "netcat localhost 5135"
            put (hinput,houtput)
            r <- m
            liftIO $ do hPutStr hinput "\\q"
                        hFlush  hinput
                        hClose  hinput
                        hClose  houtput
                        hClose  herror
            return r


psqlMliftIO :: IO a -> PsqlM a
psqlMliftIO m = PsqlM $ liftIO m
