module Logic(
	Rule(Rule)
	,AnchoredRule(AnchoredRule)
	, anchor_rule
	, File(name)
	, (#)
	, (##)
	, Params
	, Cmd
	, make
	, substitute
	, create_file
	, params_from_list
	, rules_to_tree
) where
import Data.List.Utils(join)
import Data.Maybe
import Hdfs
import Data.Time.LocalTime

import qualified Data.Map.Strict as Map

data File = File { name :: String  
				     , gen :: Params -> String
                     , pnames :: [String]  
                     , anchored_parameters :: Params 
                     }

--create_file::String->[(String, String)] -> String->[String] ->[(String, String)]  
create_file name gen pnames anchored_parameters = File {name = name, gen = gen, pnames = pnames, anchored_parameters=anchored_parameters}

params_from_list::[(String, String)]->Params
params_from_list = Map.fromList

anchored_name::File->String
anchored_name f = (gen f)  (anchored_parameters f)

not_anchored_params::File->[String]
not_anchored_params f =
	let anchored = Map.keys (anchored_parameters f) in
	filter (`notElem` anchored) (pnames f)

-- file_from_string s = File {name = s, pnames=[], anchored_parameters=[]}
is_fully_anchored::File -> Bool
is_fully_anchored f =
	length (not_anchored_params f) == 0

instance Show File where
    show f =  
    	let anchored = anchored_parameters f in
    	let str_anchored = map (\(a,b) -> a ++ "=" ++ b)  (Map.assocs anchored) in
    	let not_anchored = not_anchored_params f in
    	if is_fully_anchored f then
			((gen f) (anchored_parameters f)) 
		else
			(name f) ++  ":[" ++ (join "," ( not_anchored)) ++ "/" ++  (join ","  str_anchored) ++ "]" 

substitute::File->Params->File
substitute f params =
	Map.foldlWithKey aux f params
	--aux::File->String->String->String
	where aux f k v =
		if notElem k (pnames f) then error (k ++ " is not parameter of " ++ (name f)) else
		create_file (name f) (gen f) (pnames f) (Map.insert k v (anchored_parameters f) )


type Params = Map.Map String String
type Cmd = String

type CmdGen =  (Params -> ([File], Cmd))
data Rule = Rule File CmdGen
data AnchoredRule = AnchoredRule File [File] Cmd deriving Show

(#)::Params->String->String
(#) params k = case Map.lookup k params of
		Just v -> v
		_ -> error ("can't find " ++ k ++ " in " ++ (show params))

(##)::Params->(String, String)->Params
(##) params (k,v) = Map.insert k v params

anchor_rule::File->Rule->Maybe AnchoredRule
anchor_rule file (Rule output cmdGen) =
	-- paste a-1 > d 
	-- paste a-$date > c-[date=1]
	if not (is_fully_anchored file) then error ("can't anchor rule to generate abstract file " ++ (show file)) else
	let params = (anchored_parameters file) in
	

	if name output /= name file then Nothing else
	-- TODO: covers?
	-- sort a-$date > c-$date can't generate c (without any argument)
	--if pnames output /= pnames file then Nothing else

	let (inputs, cmd) = cmdGen params in
	Just (AnchoredRule (substitute output params) inputs cmd)

data Tree =  Node AnchoredRule [Tree] | Term deriving (Show)

rules_to_tree::[Rule]->File->Tree
rules_to_tree rules f =
    case mapMaybe (anchor_rule f) rules of
        [] -> Term
        rule@(AnchoredRule output inputs cmd):_ -> Node rule (map (rules_to_tree rules) inputs )

should_be_rerun_by_stat::Maybe LocalTime->[Maybe LocalTime]->Bool
-- if the output doesn't exist -> must run
should_be_rerun_by_stat Nothing _ = True
should_be_rerun_by_stat (Just time) inputs = 
	foldl needed False inputs where
		needed True _ = True
		needed _ Nothing = True
		needed _ (Just t) 
			| t > time = True
			| otherwise = False

stat_file = last_modified2

should_be_rerun::AnchoredRule->IO Bool    
should_be_rerun (AnchoredRule output inputs _) =
	do
		o <- stat_file $ anchored_name output
		dates <-  sequence (map stat_file  (map anchored_name inputs))
		return (should_be_rerun_by_stat o dates)

-- selects subtree which should be executed
select::Tree->IO Tree
select Term = return Term
select (Node r childs) = -- this must be executed if any of it's child
		do
                x <- should_be_rerun r
                c <- sequence $ map select childs
                let ct = filter is_not_term c in
                 if (null ct) && (not x) then
                    return Term
                 else 
                    return (Node r ct)
                 where
                 -- maybe there is a shorter version of this
                 is_not_term Term = False
                 is_not_term _ = True 

flatten :: [[a]] -> [a]
flatten l = foldl (++) [] l

execution::Tree->IO [AnchoredRule]
execution Term = return []
execution (Node r childs) = do
		c <- sequence $ map execution childs
		return ( (flatten c) ++ [r])

make::[Rule]->File->IO [AnchoredRule]
make rules file =  do
	s <- (select $ rules_to_tree rules file)
	execution s
	

--	let tree = rules_to_tree rules file in
--				   let selected = select tree in
--				   execution selected

instance Show Rule where
    show (Rule output cmdGen) = 
    	let (inputs, cmd) = cmdGen ( anchored_parameters output ) in
    	show output ++  "->" ++ " " ++ (join " " ( map show inputs )) 

