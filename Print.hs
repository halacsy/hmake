module Print 
where
import Prelude hiding (filter)
import Language hiding (join)
import Data.List hiding (filter, group, groupBy)
import Schema
import Language hiding (join)
import Data.Maybe (mapMaybe)
import Data.HashTable (hashString)
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
    pprint (Sum e) = "SUM (" ++ pprint (Selector e) ++ ")"
    pprint (Count e) = "COUNT(" ++ pprint (Selector e) ++ ")"
    pprint (Selector (ComplexSelector sel1 sel2)) = pprint (Selector sel1) ++ "." ++ pprint (Selector sel2)
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
typ2str (B s) = "bag {T:("  ++ (schema2str s) ++ ")}"
typ2str (T s) = "tuple ("  ++ (schema2str s) ++ ")"
typ2str L = "long"
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

forechExps2Str::[GeneratingExp]->String
forechExps2Str xs = join "," (map printNamedExp xs)
    where       
        printNamedExp::GeneratingExp->String
        -- pig doesn't like if we write x as x   
        -- like hereAAAA =  FOREACH AAA GENERATE group as group,COUNT($1) as count;
        printNamedExp (Exp (Selector (Name s)) (Just  name) )
                            | s == name = s
                            | otherwise = s ++ " as " ++ name
        printNamedExp (Exp exp (Just name)) = pprint exp ++ " as " ++ name
        printNamedExp (Exp exp Nothing) = pprint exp 
        printNamedExp (NestedProjection sel xs) = pprint (Selector sel) ++ ".(" ++ join "," (map (pprint . Selector) xs) ++ ") as elements"
        printNamedExp (Flatten selector) = "FLATTEN(" ++ pprint (Selector selector) ++ ")"
        

mapChars '/' = 'p'
mapChars '*' = '_'
mapChars '{' = '_'
mapChars '}' = '_'
mapChars ',' = '_'
mapChars '.' = '_'
mapChars '-' = '_'



mapChars c = c

name2Alias name = "A"++ show (abs $ hashString name)

-- TODO: lehet ket load is
pp::Node->(Ident, String)
pp (InputFile schema (PigFile file)) = let alias = name2Alias file in 
                         (alias, alias ++ " = LOAD '" ++ file ++ "' USING PigStorage(' ') AS (" ++ (schema2str schema) ++ ");")

-- we load if it was cached
pp (Transformer (Just file) schema _  ) = pp (InputFile schema file)

-- in any other case we generate the pig script
pp (Transformer _ _ (GroupBy exps pipe)) = pp_pipe (\i -> " GROUP " ++ i  ++ " BY " ++ (exps2str True exps)) pipe
pp (Transformer _ _ (Generate exps pipe)) = pp_pipe (\i -> " FOREACH " ++ i  ++ " GENERATE " ++ (forechExps2Str  exps)) pipe
pp (Transformer _ _ (Filter cond pipe)) = pp_pipe (\i -> " FILTER " ++ i  ++ " BY " ++ (pprint cond)) pipe
pp (Transformer _ _ (Distinct pipe)) = pp_pipe (\i -> " DISTINCT " ++ i) pipe

pp (Transformer _ _ (Join typ pipe1 sel1 pipe2 sel2)) = (ident, prev_text ++ "\n" ++ this_text)
                 where
                    (pident1, prev_text1) = pp pipe1
                    (pident2, prev_text2) = pp pipe2
                    prev_text = prev_text1 ++ "\n" ++ prev_text2
                    this_text = ident ++ " = " ++ "JOIN " ++ pident1 ++ " BY " ++ (pprint (Selector sel1)) ++ " full, " ++ pident2 ++ " BY " ++ (pprint (Selector sel2)) ++ ";"
                    ident = pident1 ++ pident2;


pp (Transformer _ _ (Union nodes)) = 
        (ident, predString ++ "\n" ++ ident ++ " = UNION " ++ predAliases ++ " ;")
    where
        ident = name2Alias predAliases
        predAliases = join "," (map fst preds)
        aliases = getAliasesFromNodes nodes
        load = getLoadsFromNodes nodes
        preds = (aliases ++ load)
        predString = concat (map (\(_, str) -> str ++ "\n") preds)
       
getLoadsFromNodes::[Node]->[(Ident, String)]
getLoadsFromNodes nodes =
        let files = map nameOfFile $ mapMaybe toLoadFile nodes in 
        case length files of
            0  -> []
            otherwise -> let globbed = toGlob files in 
                         [pp (InputFile (schemaOfNode $ head nodes) (PigFile globbed))]
        where
            toLoadFile (InputFile _ file) = Just file
            toLoadFile _ = Nothing


getAliasesFromNodes::[Node]-> [(Ident, String)]
getAliasesFromNodes nodes =
        map pp (mapMaybe maybeTransformer nodes)
        where
            maybeTransformer t@(Transformer _ _ _ ) = Just t
            maybeTransformer _ = Nothing

        
-- this can be more haskell like. Creates from
-- /Users/hp/log-1, /Users/hp/log-2 -> /Users/hp/log-{1,2}
toGlob::[String]->String
toGlob names =
        let 
            prefix = take prefix_len (head names)
            remainders = map (drop prefix_len) names
        in 
            if length names > 1 then
                prefix ++ "{" ++ (join "," remainders) ++ "}"
            else
                head names
    where
        prefix_len = if length names > 1 then aux 1 else 0
        aux n = let pat = take n (head names) in
              if all (\s-> pat == (take n s)) names then (aux (n + 1))  else n  - 1

-- pp_pipe::Pipe->(Ident, String)
pp_pipe f pipe = (ident, prev_text ++ "\n" ++ this_text)
                 where
                    (pident, prev_text) = pp pipe
                    this_text = ident ++ " = " ++ (f pident) ++ ";"
                    ident = pident ++ "A"


--pp (typ, (Generate exps pipe)) = 


pigScriptWithStore::Node->String->String
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