module Parser where
import Text.Parsec
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Number

readFromFile :: String -> IO (Maybe [[Int]]) 
readFromFile str= do { p <- parseFromFile application str
                     ; case p of (Left e)  -> return Nothing
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

vars = do { vs <- endBy var newline
          ; return vs
          }

var = do { string "(r"
          ; x <- int
          ; string ",c"
          ; y <- int
          ; string "):"
          ; v <- int
          ; return (x,y,v)
          }

parseVars :: String -> Maybe [(Int,Int,Int)]
parseVars  str = case  parse vars "" str of (Right c) -> return c