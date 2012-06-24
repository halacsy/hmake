module Logic(
	Rule(Rule)
	,AnchoredRule(AnchoredRule)
	, anchor_rule
	, File(name)
	, (#)
	, (##)
	, Params
	, Cmd
	, substitute
	, create_file
	, params_from_list
) where
import Data.List.Utils(join)
import Data.Maybe
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

	--simple_rule f1 cmd f2 = Rule {cmd=cmd, inputs=[file_from_string f1], output=(file_from_string f2)}
instance Show Rule where
    show (Rule output cmdGen) = 
    	let (inputs, cmd) = cmdGen ( anchored_parameters output ) in
    	show output ++  "->" ++ " " ++ (join " " ( map show inputs )) 

