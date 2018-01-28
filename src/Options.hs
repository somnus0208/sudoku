module Options where

import Data.Maybe
import Data.List
import System.Directory
import System.IO

import Propositional.NormalForm

import Solver
import Propositional.MSatPort
import Parser

data Options = Option (String, FilePath) | Help | Usage | Version 

instance Show Options where
        show Help = show Usage ++ "\nOPTIONS:"
                               ++ "\n        -s        print sat result from minisat"
                               ++ "\n        -d        print dimacs for minisat input"
                               ++ "\n"
        show Usage = "USAGE: sdks -[sd] FILE\n"


parseOptions :: [String] -> Options
parseOptions args = case args of ["-h"]       -> Help
                                 ['-':os]     -> Option (os, "")
                                 ['-':os, fp] -> Option (os, fp)                                  
                                 _            -> Usage
                  
err :: Int -> String -> IO()
err 1 _ = hPutStrLn stderr  "ERROR: input file format is invalid"
err 2 _ = hPutStrLn stderr  "ERROR: file does not exist"
err 3 s = hPutStrLn stderr ("ERROR: unknown parameter(s): -" ++ s)
err 4 _ = hPutStrLn stderr  "ERROR: not enough parameter(s)"
err 5 _ = hPutStrLn stderr  "ERROR: unknown parameter(s): -"



run :: (String, FilePath) -> IO () 

run (_ , "")         = err 4 ""

run ("", (s:p))      = return ()

run ('s':os,(s:p))   = do { ie <- doesFileExist (s:p)
                          ; if not ie 
                            then do
                              err 2 ""
                              return () 
                            else do 
                               c <- readFromFile (s:p)
                               case c of Just c' -> (runMSat.toDimacs.genAllCons) c' >>= putStrLn
                                         _       -> err 1 "" 
                          ; run (os,(s:p))}

run ('d':os, (s:p))  = do { ie <- doesFileExist (s:p)
                          ; if not ie 
                            then do
                              err 2 ""
                              return () 
                            else do 
                           c <- readFromFile (s:p)
                           case c of Just c' -> putStrLn $ head [s| (_,s)<- [(toDimacs.genAllCons) c']] 
                                     _       -> err 1 ""
                          ; run (os,(s:p))}

run (c:os, (s:p))    = do
                          err 3 [c]
                          run (os, (s:p))


