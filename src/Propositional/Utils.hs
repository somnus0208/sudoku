module Propositional.Utils where

import Propositional.Core

concatWithDisjunction ::  [Expr] -> Expr
concatWithDisjunction [expr] = expr
concatWithDisjunction (expr:exprs) = disj (expr) (concatWithDisjunction exprs)

concatWith :: (Expr -> Expr -> Expr) -> [Expr] -> Expr

concatWith op [expr] = expr
concatWith op (expr:exprs) = op (expr) (concatWith op exprs)