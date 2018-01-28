module Main where
import System.Environment
import System.Exit

import Options

main :: IO ()
main = do
  args <- getArgs
  case parseOptions args of                   
    Option  os      -> run os
    options         -> print options
