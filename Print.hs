module Print 
where
import Prelude hiding (filter)
import Language
import Data.List hiding (filter, group, groupBy)

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

instance Pprint Condition where
    pprint (Comp op exp1 exp2) = "(" ++ pprint exp1 ++ pprint op ++ pprint exp2 ++ ")"
    pprint (And exp1 exp2) = "(" ++ pprint exp1 ++ " AND " ++ pprint exp2 ++ ")"
    pprint (Or exp1 exp2) = "(" ++ pprint exp1 ++ " OR " ++ pprint exp2 ++ ")"

--data Exp = IA Int | SA String | Sub Exp Exp | Selector Selector | Sum Selector | Count Selector deriving (Show, Eq)

instance   Pprint Exp where
    pprint (IA i) = show i
    pprint (SA s) = "'" ++ s ++ "'"
    pprint (Selector (Pos i)) = "$" ++ (show i)
    pprint (Selector (Name s)) = s
    
    {-
    pprint (CompExpr op exp1 exp2) = "(" ++ pprint exp1 ++ pprint op ++ pprint exp2 ++ ")"
    pprint (ArithExpr op exp1 exp2) = "(" ++ pprint exp1 ++ pprint op ++ pprint exp2 ++ ")"
    pprint (BoolExpr op exp1 exp2) = "(" ++ pprint exp1 ++ " " ++ pprint op ++ " " ++ pprint exp2 ++ ")"
    pprint (Positional i) = "$" ++ show i
    pprint (NestedPositional i j) = "$" ++ show i ++ ".$" ++ show j

    pprint (Tuple []) = ""
    pprint (Tuple x) = let s = map pprint x in "(" ++ (join ", " s) ++ ")"
    pprint (Count e) = "COUNT(" ++ pprint e ++ ")"
    pprint (Sum e) = "SUM(" ++ pprint e ++ ")"
    pprint (Flatten e) = "FLATTEN(" ++ pprint e ++ ")"

instance  Pprint ArithmeticOperator where
    pprint Add = "+"
    pprint Sub = "-"
    pprint Mul = "*"
    pprint Div = "/"
    pprint Mod = "%"
    pprint BinCond = undefined

-}

typ2str I = "int"
typ2str S = "bytearray"
typ2str (B s) = "bag {"  ++ (schema2str s) ++ "}"
typ2str (T s) = "tuple ("  ++ (schema2str s) ++ ")"

schema2str::Schema->String
schema2str rtyp = join ", " $ map namedTyp2str rtyp
        where 
            namedTyp2str (name, typ) = (case name of
                                            Just s -> s ++ ":"
                                            Nothing -> "") ++ (typ2str typ)
           



type Ident = String

exps2str::Bool->[Exp]->String
exps2str _ [] = ""
exps2str _ (x:[]) = (pprint x)
exps2str True xs = "(" ++ join "," (map pprint xs) ++ ")"
exps2str False xs = join "," (map pprint xs) 

-- TODO: lehet ket load is
pp::Pipe->(Ident, String)
pp (typ, (Load name )) = ("A", "A = LOAD '" ++ name ++ "' USING PigStorage(' ') AS (" ++ (schema2str typ) ++ ");")
pp (_, (GroupBy exps pipe)) = pp_pipe (\i -> " GROUP " ++ i  ++ " BY " ++ (exps2str True exps)) pipe
pp (_, (Generate exps pipe)) = pp_pipe (\i -> " FOREACH " ++ i  ++ " GENERATE " ++ (exps2str False exps)) pipe
pp (_, (Filter cond pipe)) = pp_pipe (\i -> " FILTER " ++ i  ++ " BY " ++ (pprint cond)) pipe
pp (_, (Distinct pipe)) = pp_pipe (\i -> " DISTINCT " ++ i) pipe

-- pp_pipe::Pipe->(Ident, String)
pp_pipe f pipe = (ident, prev_text ++ "\n" ++ this_text)
                 where
                    (pident, prev_text) = pp pipe
                    this_text = ident ++ " = " ++ (f pident) ++ ";"
                    ident = pident ++ "A"


--pp (typ, (Generate exps pipe)) = 

pigScriptWithStore::Pipe->String->String
pigScriptWithStore pipe file = 
    let (ident, str) = pp pipe in
    str ++ "\n" ++ "STORE " ++ ident ++ " INTO '" ++ file ++ "' USING PigStorage(' ') ;"

-- Generate [Exp] Pipe | GroupBy [Exp] Pipe | Filter Condition Pipe | Load String deriving (Show)
{- 
a = load "vacak" [(Just "user_id", I), (Just "prezi_id", S), (Just "freq", I)]
      >>= generate [Selector (Pos 1), Selector (Name "freq")] 
    >>= groupBy [Selector (Pos 1), Selector (Pos 0)] 
    >>= filter (Comp Eq (Selector (Pos 1)) (Selector (Pos 2)))

main = do
    case a of
        Left s -> print s
        Right p -> do
            putStr $ pigScriptWithStore p "output" 
            print $ schema2str $ schemaOfPipe p
-}