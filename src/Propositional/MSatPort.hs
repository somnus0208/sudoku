module Propositional.MSatPort where

import System.Process
import GHC.IO.Handle
import System.IO.Temp
import Debug.Trace
import Data.List

import Propositional.Core
import Propositional.MsatParser

getStdOut :: String -> IO (ProcessHandle)
getStdOut cmd = do
    (_, Just out, _, ph) <- createProcess (shell cmd) { std_out = CreatePipe }
    return ph


runMSat :: (Substition,String) -> IO (String)
runMSat (subs,cnf) = do
    fp <- writeSystemTempFile "dimacs.in" cnf
    fp_out <- emptySystemTempFile "dimacs.out"
    ph <- getStdOut ("minisat " ++ fp ++" "++ fp_out)
    waitForProcess ph
    s <- readFromFile fp_out
    let   subs_new = [(v_new,Variable v)|(v,Variable v_new)<-subs]
    case s of Nothing -> return "Unsatisfiable"
              Just m  -> return (intercalate "\n" [show var |(var_new, val) <- m,Just var <- [lookup var_new subs_new],val==True])
