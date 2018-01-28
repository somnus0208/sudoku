module Parser where
import Debug.Trace
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number

readFromFile :: String -> IO (Maybe [[Int]]) 
readFromFile str= do { p <- parseFromFile application str
                     ; case p of (Left e)  -> traceShow e  return Nothing
                                 (Right c) -> return (Just c)
                     }

application  = try slines

slines = do { char '('
            ; newline
            ; lds <- endBy sline newline
            ; char ')'
            ; return lds
            }
sline = do  { ld <- sepBy decimal (char ' ')
           ; return ld
           }
