module Solver where
import Data.List
import Data.List.Split
import Propositional.Core
import Propositional.Utils

type Cell =[[Int]]

genBoxCons :: Cell -> [Expr]
genBoxCons c =  [ concatWith (disj) [ Variable $ getVarName i j k | 
                                             k <- [1..length c] ] | 
                                             i <- [1..length c], j <- [1..length c]]

genRowCons :: Cell -> [Expr]
genRowCons c =  concat [ [ (neg $ Variable (getVarName  i j k)) `disj` (neg $ Variable (getVarName  i' j' k)) |
                           (i,j) <-row, (i', j') <- row, (i,j) /= (i',j')] | 
                           row <- rows, k <- [1..length c]]
    where
        rows = [[(i,j) | j <- [1..length c]] | i <- [1..length c]]

genColCons :: Cell -> [Expr]
genColCons c = concat [ [ (neg $ Variable (getVarName  i j k)) `disj` (neg $ Variable (getVarName  i' j' k)) |
                           (i,j) <-col, (i', j') <- col, (i,j) /= (i',j')] | 
                           col <- cols, k <- [1..length c]]
    where
        cols = [[(i,j) | i <- [1..length c]] | j <- [1..length c]]

genCelCons :: Cell -> [Expr]
genCelCons c = concat[ [ Variable $ getVarName i j k | (k, j) <- zip ks [1..], k /= 0] | (ks,i) <- zip c [1..]]

genCagCons :: Cell -> [Expr]
genCagCons c = concat [ [ (neg $ Variable (getVarName i j k)) `disj` (neg $ Variable (getVarName  i' j' k)) |
                          (i,j) <-cas, (i', j') <- cas, (i,j) /= (i',j')] | 
                          cas <- css, k <- [1..length c]]
    where
        pow = (round.sqrt.fromIntegral) (length c)        
        ijs = chunksOf pow [1..length c]
        css = [[ (i,j)| i<-is,j<-js]  | is <- ijs,js <- ijs]

genAllCons :: Cell -> Expr
genAllCons  c = concatWith conj (genBoxCons c ++ genRowCons c ++ genColCons c ++ genCelCons c ++ genCagCons c)

getVarName :: Int -> Int -> Int -> Var
getVarName i j k = Var ('(':'r':show i ++","++ 'c':show j ++ "):"++ show k)

prettyPrint :: [(Int, Int, Int)] -> String
prettyPrint vs = "(\n" ++ (prettyPrint' vs (length vs)) ++ ")"

prettyPrint' :: [(Int, Int, Int)] -> Int -> String
prettyPrint' [] _ = ""
prettyPrint' ((x,y,v):vs) c = let newline = if y * y == c then "\n" else ""
                              in show v ++" " ++ newline ++ (prettyPrint' vs c)
