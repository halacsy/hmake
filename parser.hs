module Parser
	(
		Chunk(Part, KV, K)
	 ,  parseFileName
     , fp
	 ) where

import Text.ParserCombinators.Parsec hiding (State)
import Data.Maybe
import Data.List(nub)

import Control.Monad.State(modify, State, execState)
import Logic(File, Rule, create_file, params_from_list, Params) 
import Data.List.Split(splitOn)
import Data.Maybe

import qualified Data.Map.Strict as Map

data Chunk = Part String | KV String String | K String deriving Show


--add_bi_rule i1 i2 cmd o = modify $ (:) (create_rule cmd [(fp i1),(fp i2)]  (fp o))

--add_rule i cmd o  = modify $ (:) (create_rule cmd [(fp i)]  (fp o))
--build_rules :: State ([Rule]) () -> [Rule]
--build_rules f = execState f []


fileName :: GenParser Char st [Chunk]
fileName = do 
             result <- many chunk
       	     eof
       	     return result

chunk = name  <|> try kvPair  <|> try keyOnly

name = do 
		s <-  many1 (noneOf ",\n=$")
		return (Part s)

kvPair =
	do k <- key 
	   _ <- char '=' 
	   v <- value
	   return (KV  k v)

keyOnly = do
	s <- key
	return (K s)

key :: GenParser Char st String
key = do
	d <- dollar
	many (noneOf "=,\n$-/_")

value :: GenParser Char st String
value =
	many (noneOf ",\n=/$-_")

dollar = char '$'
eq :: GenParser Char st Char
eq = char '='


parseFileName :: String -> Either ParseError [Chunk]
parseFileName input = parse fileName "(unknown)" input


match::[Chunk]->String->Params->String
match chunks name params = foldl (++) "" (map chunk_to_string chunks) where
	chunk_to_string (K k) = case Map.lookup k params of
    							  		Nothing -> error ("can't find " ++ k ++ " in " ++ (show params) ++ " for file " ++ name) 
    							  		Just v -> v
    	chunk_to_string (Part p) = p
    	chunk_to_string (KV k v) = v

fp s = 
	-- this should be shorted
    let chunks = case parseFileName s  of
            (Left err) -> error (show err)
            (Right r) -> r
    in
    let collectParam chunk = case chunk of 
                        K k -> Just k
                        KV k v -> Just k
                        _ -> Nothing in
    let params = nub $ mapMaybe collectParam chunks in
    let collectKVs chunk = case chunk of
                        KV k v -> Just (k,v)
                        _ -> Nothing in
    let kvs = nub $ mapMaybe collectKVs chunks in
    let collectNames chunk = case chunk of
                        Part s -> Just s
                        _ -> Nothing in
    let name = foldl (++) "" (mapMaybe collectNames chunks) in
  
    create_file name (match chunks name)  params (params_from_list kvs)


--make f r = hamake (fp f) r

main = do
 	print (parseFileName "monthly_uniq_users-$year=2012-$month=01") 