module Propositional.Parser where

import Propositional.Core
import Data.Maybe
import Text.ParserCombinators.Parsec 
import Text.ParserCombinators.Parsec.Error
import Text.ParserCombinators.Parsec.Pos
import Text.ParserCombinators.Parsec.Prim

readExpr ::  String -> Maybe Expr
readExpr str = let r = parseExpr  "" str
               in case  r of (Right e) -> Just e
                             _       -> Nothing     


parseExpr :: SourceName -> String -> Either ParseError Expr
parseExpr = parse statement

statement :: GenParser Char st Expr
statement = do {spaces
           ; x <- try binary <|> expr
           ; spaces
           ; eof
           ; return x}

expr :: GenParser Char st Expr
expr = choice [binaryP, negation, variable]

variable :: GenParser Char st Expr
variable = do {s <- many1 letter
          ; return $ Variable (Var s)
          }
negation :: GenParser Char st Expr
negation = do {char '~'
          ; spaces
          ;x <- expr
          ;return $ Negation x}

binaryP :: GenParser Char st Expr
binaryP = do {char '('
         ;spaces
         ;x <- binary
         ;spaces
         ;char ')'
         ;return x}

binary :: GenParser Char st Expr
binary = do {x1 <- expr
        ;spaces
        ;s  <- choice $ map string ["&", "|", "->", "<->"]
        ;spaces
        ;x2 <- expr
        ;return $ connective s x1 x2}
  where
    connective c = case c of "&"   -> Conjunction
                             "|"   -> Disjunction
                             "->"  -> Conditional
                             "<->" -> Biconditional
                             _     -> error "Impossible case"
