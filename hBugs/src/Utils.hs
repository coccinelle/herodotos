module Utils where

import Control.Monad.Error

import Data.Char
import Data.List

import System.Posix
import System.Process
import System.IO

-- Debug Messages ((un)comment to turn on/off)
debug :: Bool -> String -> IO ()
debug v s = if v
            then putStr s >> hFlush stdout
            else return ()

debugLn :: Bool -> String -> IO ()
debugLn v s = debug v $ s ++ "\n"

-- Does the Maybe is a Just?
isJust :: Maybe t -> Bool
isJust  Nothing = False
isJust (Just _) = True

-- Run a command and give its output
runCmd :: String -> IO String
runCmd cmd = do (filepath, handle) <- mkstemp "tables_tmp_XXXXXX"
                hClose handle
                system $ cmd ++ " > " ++ filepath
                str <- readFile filepath
                removeLink filepath
                return str


sepPred :: (a -> Bool) -> [a] -> [[a]]
sepPred p l = aux [] [] $ reverse l
  where
    aux res acc []      = acc : res
    aux res acc (x : q) = if p x
                            then aux (acc : res) [] q
                            else aux  res (x : acc) q


foldr2 :: (t -> t -> t) -> t -> [t] -> t
foldr2 _ init   []      = init
foldr2 _ _      [x]     = x
foldr2 f init   (x : l) = f x (foldr2 f init l)


mapFilterEither :: (a -> Either e b) -> [a] -> [b]
mapFilterEither f []      = []
mapFilterEither f (x : l) = case f x of
                              Right y -> y : (mapFilterEither f l)
                              _       ->     (mapFilterEither f l)


hReadFull :: Bool -> Handle -> IO [String]
hReadFull v h = do debug v "=> Reading : "
                   aux v h
  where
    aux v h = do debug v "=> Testing EOF : "
                 iseof <- hIsEOF h
                 debugLn v (show iseof)
                 if iseof
                   then return []
                   else do debug v "=> Getting Line : "
                           l <- hGetLine h
                           debug v (show l)
                           if null l
                             then return []
                             else do q <- aux v h
                                     return $ l : q



-- Name Normalize
normalizeName :: String -> String
normalizeName s = map f s
  where f c = if isAlphaNum c
               then c
               else '_'
