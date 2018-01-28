module Propositional.Core where

import Data.List

newtype Var = Var String deriving (Eq,Ord) 

instance Show Var where
    show (Var v) = v

data Expr = Variable      Var 
      | Negation      Expr
      | Conjunction   Expr Expr
      | Disjunction   Expr Expr
      | Conditional   Expr Expr
      | Biconditional Expr Expr deriving Eq

instance Ord Expr where
  compare e1 e2 
    | e1 == e2 = EQ
    | depth (e1) > depth (e2) = GT
    | otherwise  = LT

instance Show Expr where
  show (Variable      name)      = show name
  show (Negation      expr)      = '¬' : show expr
  show (Conjunction   exp1 exp2) = showBC "∧" exp1 exp2
  show (Disjunction   exp1 exp2) = showBC "∨" exp1 exp2
  show (Conditional   exp1 exp2) = showBC "→" exp1 exp2
  show (Biconditional exp1 exp2) = showBC "↔" exp1 exp2


type Substition = [(Var, Expr)]

showBinaryConnective :: (Expr -> String) -> String -> Expr -> Expr -> String
showBinaryConnective show_ symbol exp1 exp2 =
  '(' : show_ exp1 ++ " " ++ symbol ++ " " ++ show_ exp2 ++ ")"

showBC :: String -> Expr -> Expr -> String
showBC = showBinaryConnective show

neg :: Expr -> Expr
neg = Negation

disj :: Expr -> Expr -> Expr
disj = Disjunction

conj :: Expr -> Expr -> Expr
conj = Conjunction

cond :: Expr -> Expr -> Expr
cond = Conditional

var :: String-> Expr
var  s = Variable (Var s)

bicon :: Expr -> Expr -> Expr
bicon = Biconditional

true :: Var -> Expr
true = Variable

false :: Var -> Expr
false c = neg $ Variable c

depth :: Expr -> Int
depth (Variable _) = 0
depth (Negation subExpr) = 1 + depth subExpr
depth (Conjunction subExpr1 subExpr2) = 1 + maximum [depth subExpr1, depth subExpr2]
depth (Disjunction subExpr1 subExpr2) = 1 + maximum [depth subExpr1, depth subExpr2]
depth (Conditional subExpr1 subExpr2) = 1 + maximum [depth subExpr1, depth subExpr2]
depth (Biconditional subExpr1 subExpr2) = 1 + maximum [depth subExpr1, depth subExpr2]

substitute :: Expr -> Substition -> Expr
substitute expr@(Variable v) sub       = let val = lookup v sub
                                    in case val of (Just expr_) -> expr_
                                                   _            -> expr   
substitute (Negation subExpr) sub = neg $ substitute subExpr sub
substitute (Conjunction subExpr1 subExpr2) sub = (substitute subExpr1 sub) `conj` (substitute subExpr2 sub)
substitute (Disjunction subExpr1 subExpr2) sub = (substitute subExpr1 sub) `disj` (substitute subExpr2 sub)
substitute (Conditional subExpr1 subExpr2) sub = (substitute subExpr1 sub) `cond` (substitute subExpr2 sub)
substitute (Biconditional subExpr1 subExpr2) sub = (substitute subExpr1 sub) `bicon` (substitute subExpr2 sub)


getVars = nub.getVars'
getVars' :: Expr -> [Var]
getVars' (Variable v) = [v]
getVars' (Negation subExpr) = getVars subExpr
getVars' (Conjunction subExpr1 subExpr2) = getVars subExpr1 ++ getVars subExpr2
getVars' (Disjunction subExpr1 subExpr2) = getVars subExpr1 ++ getVars subExpr2
getVars' (Conditional subExpr1 subExpr2)  = getVars subExpr1 ++ getVars subExpr2
getVars' (Biconditional subExpr1 subExpr2) = getVars subExpr1 ++ getVars subExpr2