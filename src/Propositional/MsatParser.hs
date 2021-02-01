module Propositional.MSatParser where

import Propositional.Core
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number

import Debug.Trace

readFromFile :: String -> IO (Maybe [(Var, Bool)])
readFromFile str = do 
                    res <- parseFromFile application str
                    case res of (Right e) -> return (Just e)
                                other     -> return Nothing
application ::GenParser Char st [(Var, Bool)]
application = try satresult 

satresult = do
            string "SAT"
            newline
            res <- sepBy int (char ' ')
            return [(Var $ show (abs r), b)| r <- res,r /= 0,let b = if r > 0 then True else False]
