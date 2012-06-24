module Dsl (add_rule, add_bi_rule, build_rules, make, create_string_template)  where
import Control.Monad.State
import  Logic(File, Rule, create_file, create_rule, hamake) 
import Data.List.Split
import Parser
import Data.Maybe
import Data.List(nub)
add_bi_rule i1 i2 cmd o = modify $ (:) (create_rule cmd [(create_string_template i1),(create_string_template i2)]  (create_string_template o))

add_rule i cmd o  = modify $ (:) (create_rule cmd [(create_string_template i)]  (create_string_template o))
build_rules :: State ([Rule]) () -> [Rule]
build_rules f = execState f []

-- 
create_string_template2::String->File
create_string_template2 s = 
    -- this is a temporal solution, we just check the end of the string
    case splitOn "$" s of
        [] -> error "empty string"
        [n] -> create_file s (\_ -> n)  [] []
        n:p:[] ->
            let (params, anchored) = case splitOn "=" p of
                    [k] -> ([k], [])
                    k:v:[] -> ([k], [(k,v)])
            in 
            create_file n (match_template (head params) n) params anchored
            where 
                match_template pname fname params = case lookup pname params  of
                                  Nothing -> error ("can't find " ++ pname ++ " in " ++ (show params) ++ " for file " ++ fname)
                                  Just v -> n++v

create_string_template::String->String

create_string_template s = 
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
    name

--make f r = hamake (create_string_template f) r

main = do
    print (create_string_template "run$date=2/$d")
-- rule a "grep vacak"  b

--cmd (a date=1,year=2012) b > output
