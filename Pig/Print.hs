module Pig.Print 
(
	pretty_print
) where

import Pig.Language
import Data.List hiding (filter, group)

join delim l = concat (intersperse delim l)

class Pprint a where
  pprint :: a -> String
  	
	
instance  Pprint ComparisonOperator where
	pprint Eq  = " == "
	pprint Neq = " != "
	pprint Lt  = " < "
	pprint Gt  = " > "
	pprint LtE = " <= "
	pprint GtE = " >= "
	pprint Matches = " matches "

instance Pprint BoolOperator where
	pprint Or = "OR"
	pprint And = "AND"

instance   Pprint Expr where
	pprint (CompExpr op exp1 exp2) = "(" ++ pprint exp1 ++ pprint op ++ pprint exp2 ++ ")"
	pprint (ArithExpr op exp1 exp2) = "(" ++ pprint exp1 ++ pprint op ++ pprint exp2 ++ ")"
	pprint (BoolExpr op exp1 exp2) = "(" ++ pprint exp1 ++ " " ++ pprint op ++ " " ++ pprint exp2 ++ ")"
	pprint (Positional i) = "$" ++ show i
	pprint (IntExpr i) = show i
	pprint (StringExpr s) = "'" ++ s ++ "'"
	pprint (Tuple []) = ""
	pprint (Tuple x) = let s = map pprint x in join ", " s  
	pprint (Count e) = "COUNT(" ++ pprint e ++ ")"


instance  Pprint ArithmeticOperator where
	pprint Add = "+"
	pprint Sub = "-"
	pprint Mul = "*"
	pprint Div = "/"
	pprint Mod = "%"
	pprint BinCond = undefined

pprint_sub::String->PigExpr->String->String->String->String
pprint_sub v  e fname by by_or_generate = (aux subv e) ++ "\n" ++ v ++ " = " ++ fname ++ " " ++ subv ++ " " ++ by_or_generate ++" " ++ by ++ ";"
						where subv = v ++ head v:""

aux::String->PigExpr->String 
aux v (Load name _) = v ++ " = LOAD '" ++ name ++ "' USING PigStorage(' ');";
aux v (Store name e) =  (aux subv e) ++ "\n" ++ "STORE " ++ subv ++ " INTO '" ++ name ++ "' USING PigStorage(' ');"
						where subv = v ++ head v:""
aux v (Distinct e) = (aux subv e) ++ "\n" ++ v ++ "= DISTINCT " ++ subv ++ ";"
						where subv = v ++ head v:""
						
aux v (Group by  e) = pprint_sub v e "GROUP" (pprint by)  "BY"
aux v (Filter cond e) = pprint_sub v e "FILTER" (pprint cond) "BY"
aux v (Foreach e gen) = pprint_sub v e "FOREACH" (pprint gen) "GENERATE"

pretty_print::PigExpr->String
pretty_print = aux "A"