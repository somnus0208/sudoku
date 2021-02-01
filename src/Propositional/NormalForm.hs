module Propositional.NormalForm where

import qualified Data.Set as Set
import Data.List
import Propositional.Core
import Propositional.Utils
import Debug.Trace

toNNF :: Expr -> Expr
toNNF expr@(Variable _)                    = expr
toNNF expr@(Negation (Variable _))         = expr
toNNF (Negation (Negation expr))           = expr

toNNF (Conjunction exp1 exp2)              = toNNF exp1 `conj` toNNF exp2
toNNF (Negation (Conjunction exp1 exp2))   = toNNF $ neg exp1 `disj` neg exp2

toNNF (Disjunction exp1 exp2)              = toNNF exp1 `disj` toNNF exp2
toNNF (Negation (Disjunction exp1 exp2))   = toNNF $ neg exp1 `conj` neg exp2

toNNF (Conditional exp1 exp2)              = toNNF $ neg exp1 `disj` exp2
toNNF (Negation (Conditional exp1 exp2))   = toNNF $ exp1 `conj` neg exp2

toNNF (Biconditional exp1 exp2)            = let a = neg exp1 `disj` exp2
                                                 b = exp1 `disj` neg exp2
                                             in toNNF $ a `conj` b
toNNF (Negation (Biconditional exp1 exp2)) = let a = exp1 `disj` exp2
                                                 b = neg exp1 `disj` neg exp2
                                             in toNNF $ a `conj` b

toCNF :: Expr -> Expr
toCNF expr = let nnf = toNNF expr
             in if isCNF nnf then nnf else concatWith conj (tseitin nnf)

isCNF :: Expr -> Bool
isCNF (Variable _)   = True
isCNF (Negation (Variable _)) = True
isCNF (Disjunction (Variable _) (Variable _)) = True
isCNF (Disjunction (Negation (Variable _)) (Variable _)) = True
isCNF (Disjunction (Variable _) (Negation (Variable _))) = True 
isCNF (Conjunction expr1 expr2) = isCNF expr1 && isCNF expr2
isCNF _  = False

unpackCNF :: Expr -> [Expr]
unpackCNF (Conjunction e1 e2) = (unpackCNF e1) ++ (unpackCNF e2)
unpackCNF expr                = [expr] 

toDimacs :: Expr -> (Substitution,String)
toDimacs expr = let  exprs = unpackCNF expr
                     (subs,exprs_) = rename exprs
                     varNumber   = (length.nub.concat) $ map getVars exprs 
                     exprNumber  =length exprs_
                     firstLine = "p cnf " ++ (show varNumber) ++ (' ':show exprNumber) ++ "\n"  
                     constraints = intercalate  "\n" [toDimacs' expr ++ " 0" | expr <- exprs_]
                 in  (subs, firstLine ++ constraints)

toDimacs' :: Expr -> String
toDimacs' (Variable v) = show v
toDimacs' (Negation (Variable v)) = "-" ++ show v
toDimacs' (Disjunction e1 e2) = case (e1, e2) of (Variable v1,Variable v2) -> show v1 ++ (' ':show v2)
                                                 (Variable v1,e2)           -> show v1 ++ (' ':toDimacs' e2)
                                                 (e1,Variable v2)          ->toDimacs' e1 ++ (' ':show v2)
                                                 (e1,e2)                   ->toDimacs' e1 ++ (' ' : toDimacs' e2)

rename :: [Expr] -> (Substitution,[Expr])
rename exprs = let vars = (nub.concat) [getVars expr | expr <- exprs]
                   new_var_exprs = [var $ show ind| ind<-[1..]]
                   subs = zip vars new_var_exprs 
               in  (subs, [substitute expr subs | expr <- exprs])
 
tseitin :: Expr -> [Expr]
tseitin expr =let itself = lookup expr $ getRemaned expr
              in case  itself of (Just v) -> (Variable v) : simplifyAll expr
                                 _        -> []
  where
    getRemaned = insertNewVariable.getSubExpr
    simplifyAll = simplify.addExpr

getSubExpr :: Expr -> Set.Set Expr
getSubExpr expr@(Variable _)                      = Set.empty
getSubExpr expr@(Negation subExpr)                = expr `Set.insert` ( getSubExpr subExpr) 
getSubExpr expr@(Disjunction subExpr1 subExpr2)   = expr `Set.insert` ( getSubExpr subExpr1 `Set.union`getSubExpr subExpr2)
getSubExpr expr@(Conjunction subExpr1 subExpr2)   = expr `Set.insert` ( getSubExpr subExpr1 `Set.union`getSubExpr subExpr2)
getSubExpr expr@(Conditional subExpr1 subExpr2)   = expr `Set.insert` ( getSubExpr subExpr1 `Set.union`getSubExpr subExpr2)
getSubExpr expr@(Biconditional subExpr1 subExpr2) = expr `Set.insert` ( getSubExpr subExpr1 `Set.union`getSubExpr subExpr2)

insertNewVariable :: Set.Set Expr -> [(Expr, Var)]
insertNewVariable setExpr = insertNewVariable' (Set.toDescList setExpr) 1

insertNewVariable' :: [Expr] -> Int -> [(Expr, Var)]
insertNewVariable' [] _          = []
insertNewVariable' (expr:exprs) ind = (expr,Var ('@':show ind)):(insertNewVariable' exprs ( ind +1))   


convertExpr :: [(Expr, Var)] -> Expr -> Expr
convertExpr _ expr@(Variable _)               = expr

convertExpr mapExpr (Negation subExpr) = let val = lookup subExpr mapExpr                                             
                                         in case val of Just v  -> neg (Variable v) 
                                                        _       -> neg (convertExpr mapExpr subExpr)

convertExpr mapExpr (Conjunction subExpr1 subExpr2)   = let val1 = lookup subExpr1 mapExpr
                                                            val2 = lookup subExpr2 mapExpr                                           
                                                        in case (val1,val2) of (Just v1,Just v2)  -> conj (Variable v1) (Variable v2) 
                                                                               (_      ,Just v2)  -> conj (convertExpr mapExpr subExpr1) (Variable v2)
                                                                               (Just v1,_      )  -> conj (Variable v1) (convertExpr mapExpr subExpr2) 
                                                                               _                  -> conj (convertExpr mapExpr subExpr1) (convertExpr mapExpr subExpr2)

convertExpr mapExpr (Disjunction subExpr1 subExpr2)   = let val1 = lookup subExpr1 mapExpr
                                                            val2 = lookup subExpr2 mapExpr                                           
                                                        in case (val1,val2) of (Just v1,Just v2)  -> disj (Variable v1) (Variable v2) 
                                                                               (_      ,Just v2)  -> disj (convertExpr mapExpr subExpr1) (Variable v2)
                                                                               (Just v1,_      )  -> disj (Variable v1) (convertExpr mapExpr subExpr2) 
                                                                               _                  -> disj (convertExpr mapExpr subExpr1) (convertExpr mapExpr subExpr2)

convertExpr mapExpr (Conditional subExpr1 subExpr2)   = let val1 = lookup subExpr1 mapExpr
                                                            val2 = lookup subExpr2 mapExpr                                           
                                                        in case (val1,val2) of (Just v1,Just v2)  -> cond (Variable v1) (Variable v2) 
                                                                               (_      ,Just v2)  -> cond (convertExpr mapExpr subExpr1) (Variable v2)
                                                                               (Just v1,_      )  -> cond (Variable v1) (convertExpr mapExpr subExpr2) 
                                                                               _                  -> cond (convertExpr mapExpr subExpr1) (convertExpr mapExpr subExpr2)

addExpr :: Expr -> [Expr]
addExpr expr = let setExpr = getSubExpr expr
                   mapExpr = insertNewVariable setExpr
               in  addExpr' (Set.toDescList setExpr) mapExpr
 
addExpr' :: [Expr] -> [(Expr, Var)] -> [Expr]
addExpr' [] _ = []
addExpr' (expr:exprs) mapExpr = 
                                let val = lookup expr mapExpr
                                    expr_ = convertExpr mapExpr expr
                                in case val of (Just v) -> (bicon expr_ $ Variable v) : (addExpr' exprs mapExpr)
                                        
                                              
simplify :: [Expr] -> [Expr]
simplify exprs = concat [simplify' expr | expr <- exprs]

simplify' :: Expr -> [Expr]
simplify' (Biconditional (Conjunction e1 e2) e3) =  [(toNNF $ neg e1) `disj` (toNNF $ neg e2) `disj` e3,
                                                     e1 `disj` (toNNF $ neg e3), e2 `disj` (toNNF $ neg e3)]
simplify' (Biconditional (Disjunction e1 e2) e3) =  [(toNNF $ neg e1) `disj` e3,(toNNF $ neg e2) `disj` e3,
                                                    e1 `disj` e2 `disj` (toNNF $ neg e3)]
simplify' (Biconditional e1 e2)                  =  [(toNNF $ neg e1) `disj` e2]

f = conj (disj (var "p") (var "q")) (conj (disj (neg $ var "p") (var "r")) (var "s") )