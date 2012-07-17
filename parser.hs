module Parser
	 where
import Template
import Text.ParserCombinators.Parsec hiding (State)
import Data.Maybe
import Data.List(nub)
import Language.Haskell.TH
import Control.Monad.State(modify, State, execState)
import Logic
import Data.List.Split(splitOn)
import Data.Maybe

import qualified Data.Map.Strict as Map

data Expr = Expr String [String] deriving Show

data Chunk = Part String | KV String String | K String | KF String Expr deriving Show


--add_bi_rule i1 i2 cmd o = modify $ (:) (create_rule cmd [(fp i1),(fp i2)]  (fp o))

--add_rule i cmd o  = modify $ (:) (create_rule cmd [(fp i)]  (fp o))
--build_rules :: State ([Rule]) () -> [Rule]
--build_rules f = execState f []



fileName :: GenParser Char st [Chunk]
fileName = do 
             result <- many chunk
       	     eof
       	     return result

chunk = part  <|> try kvPair <|> try kfPair <|> try keyOnly

part = do 
		s <-  many1 (noneOf "(),\n=$")
		return (Part s)

kvPair =
	do k <- key 
	   _ <- char '=' 
	   v <- value
	   return (KV  k v)

kfPair =
    do k <- key
       _ <- char '='
       f <- expr
       return (KF k f)

expr = 
    do
        _ <- char '('
        fun <- many1 (noneOf "()\n ")    
        params <- many key
        _ <- char ')'    
        return (Expr fun params)


keyOnly = do
	s <- key
	return (K s)

key :: GenParser Char st String
key = do _ <- spaces
	 d <- dollar
	 many1 (noneOf "=,\n$-/_() ")

value :: GenParser Char st String
value = do
       _ <- spaces
       many1 (noneOf "=,\n$-/_() ")
        

dollar = char '$'
eq :: GenParser Char st Char
eq = char '='


parseFileName :: String -> Either ParseError [Chunk]
parseFileName input = parse fileName "(unknown)" input


match2::[Chunk]->String->Params->String
match2 chunks name params = foldl (++) "" (map chunk_to_string chunks) where
	chunk_to_string (K k) = case Map.lookup k params of
    							  		Nothing -> error ("can't find " ++ k ++ " in " ++ (show params) ++ " for file " ++ name) 
    							  		Just v -> v
    	chunk_to_string (Part p) = p
    	chunk_to_string (KV k v) = v
        chunk_to_string (KF k e) = k

-- returns a \p -> fun_name p function


fp::String->(File, String, Expr)
fp s = 
	-- this should be shorted
    let chunks = case parseFileName s  of
            (Left err) -> error (show err)
            (Right r) -> r
    in
    let collectParam chunk = case chunk of 
                        K k -> Just k
                        KV k _ -> Just k
                        KF k _ -> Just k
                        _ -> Nothing in
    -- nub == sort | uniq 
    let params = nub $ mapMaybe collectParam chunks in

    let collectKVs chunk = case chunk of
                        KV k v -> Just (k,v)
                        _ -> Nothing in
    let kvs = nub $ mapMaybe collectKVs chunks in

    let collectNames chunk = case chunk of
                        Part s -> Just s
                        _ -> Nothing in
    let name = foldl (++) "" (mapMaybe collectNames chunks) in
    -- TODO, eliminate heads
    let collectKFs chunk = case chunk of
                        KF name expr -> Just (name, expr)
                        _ -> Nothing in
    let (fname, pexpr) = head $ mapMaybe collectKFs chunks in

    let file_name_generator = let chunks' = map (\c -> case c of 
                                                    KF name _ -> K name 
                                                    x -> x) chunks in
         match2 chunks' name 
    in
    (create_file name file_name_generator  params (params_from_list kvs), fname, pexpr)
   

funnel file oparam function iparam cmd =
    \params ->
        ((map (\v -> substitute file (params##(oparam, v)))) (function (params#iparam)) , cmd)

-- cat input[input_param=generate(output_param)] | cmd > output(output_param)
funnel_rule what from_what output_param generate input_param cmd =
    (Rule what ( funnel  from_what output_param generate input_param cmd))

parse_output str = 
    let (f, _, _) = fp str in f

days_of_month::String->[String]
days_of_month y  =  map show [1..5]

cmd = "cat"
(input_template, running_param, (Expr fname params)) = fp "daily_uniq_users-$year=2012-$month=01-$day=(day_of_month $month)" 
p = head params
output = (parse_output "monthly-$year-$month")
func = days_of_month

r = funnel_rule output input_template running_param func p cmd

rule:: Q Exp
rule = 

    do
    return $ AppE (AppE (AppE (AppE (AppE (AppE (VarE (mkName "funnel_rule")) (VarE (mkName "output"))) (VarE (mkName "input_template"))) (VarE $ mkName "running_param")) (VarE $ mkName funname )) (VarE $ mkName "p")) (VarE $ mkName "cmd")
