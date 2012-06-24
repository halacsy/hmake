import Data.List.Split
import Data.Map as Map
import Control.Monad.State

type Command = String
type File = String

type Name = String
data Value = SValue String | IValue Int deriving (Show)
data Param = Param Name Value deriving (Show)
type Params = [Param]
type ParamNames = [Name]
data ParametricFile = ParametricFile Name ParamNames 
instance Show ParametricFile where
    show (ParametricFile name param_names) = name ++ ":" ++ (show param_names)

type FileListGen = Params->[File]

data AnchoredParametricFile = AnchoredParametricFile ParametricFile Params
instance Show AnchoredParametricFile where
    show (AnchoredParametricFile (ParametricFile name _) params) = name ++ ":" ++ (show params)
-- 

data ParametricRule = ParametricRule Command [ParametricFile] ParametricFile deriving (Show)
data AnchoredRule = AnchoredRule Command [AnchoredParametricFile] AnchoredParametricFile  deriving (Show)
data Tree =  Node AnchoredRule [Tree] | Term deriving (Show)

--data Rule = AnchoredRule | ParametricRule deriving (Show)
-- if f1 is newer than f2 then we need to regenarete f2
triggers::File->File->Bool
triggers  "a-2012" "b-2012" = True 
triggers "a-3" "b-3" = False
triggers _ _ = True

are_params_covered::ParamNames->Params->Bool
are_params_covered names params = and (map (member params) names)

get_name::AnchoredParametricFile->Name
get_name (AnchoredParametricFile (ParametricFile name _ ) _) = name

get_param_names::AnchoredParametricFile->ParamNames
get_param_names (AnchoredParametricFile (ParametricFile  _ param_names ) _) = param_names

get_params::AnchoredParametricFile->Params
get_params (AnchoredParametricFile _ params) = params

generates::AnchoredParametricFile->AnchoredRule->Bool
generates f (AnchoredRule cmd ins out) = (get_name f) == (get_name out) && (are_params_covered (get_param_names f) (get_params out))

could_generate::AnchoredParametricFile->ParametricRule->Bool
could_generate (AnchoredParametricFile (ParametricFile  name param_names ) params) (ParametricRule cmd inputs out) = 

find_generator_rule::[ParametricRule]->AnchoredParametricFile->AnchoredRule
find_generator_rule rules apf =


rules_to_tree::[AnchoredRule]->AnchoredParametricFile->Tree
rules_to_tree rules f =
    case filter (generates f) rules of
        [] -> Term
        [r@(AnchoredRule _ inputs _)] -> Node r (map (rules_to_tree rules) inputs)
        _ -> error ((get_name f) ++ " is generated by >1 rules")

should_be_rerun (AnchoredRule _ inputs output) =
    or (map (\file -> triggers (get_name file) (get_name output)) inputs)



-- selects subtree which should be executed
select::Tree->Tree
select Term = Term
select (Node r childs) = -- this must be executed if any of it's child
                let ct = filter is_not_term (map select childs) in
                if (null ct) && not (should_be_rerun r) then
                    Term
                else 
                    (Node r ct)
                where
                -- maybe there is a shorter version of this
                is_not_term Term = False
                is_not_term _ = True 

flatten :: [[a]] -> [a]
flatten l = foldl (++) [] l

execution::Tree->[AnchoredRule]
execution Term = []
execution (Node r childs) = (flatten (map execution childs)) ++ [r]

-- let's build a parametric RuleSet
-- sort a-$date > b-$date
-- sort c > d
-- paste b-$date d > e-$date
conts x _ = x

get_value::Params->Name->Value
get_value params n =
    case filter (\(Param n' _) -> n' == n) params of
        [] -> error "not found"
        (Param _ v):xs ->v

member::Params->Name->Bool
member [] _ = False
member ((Param n _):xs) name = if n==name then True else member xs name

to_string::Value->String
to_string (SValue v) = v
to_string (IValue v) = show v


create_string_template::String->(Params->String)
create_string_template s = 
    -- this is a temporal solution, we just check the end of the string
    case splitOn "$" s of
        [] -> error "empty string"
        [s] -> const s
        n:p:[] -> \params -> n++(to_string (get_value params p))

join_templates::[Params->String]->(Params->[String])
join_templates l = \params -> (map (\template -> template params) l)

parse_string_list::[String]->FileListGen
parse_string_list l = join_templates $ map create_string_template l

--anchor_parameter::Params->ParametricRule->Rule
--anchor_parameter params (ParametricRule command_gen file_list_gen file_gen) =
--        (Rule (command_gen params) (file_list_gen params) (file_gen params))


--mypr = [ParametricRule (conts "sort") (parse_string_list ["a-$date"]) (create_string_template "b-$date"),
--       ParametricRule (conts "sort") (parse_string_list ["c"]) (create_string_template "d"),
--        ParametricRule (conts "paste") (parse_string_list ["b-$date", "d"]) (create_string_template "e-$date")]
--params = [(Param "date" (SValue "2012"))]
--myg = (map (anchor_parameter params) mypr)

-- sort a-num > b-num
-- cat b-$(num <- [1..4]) > c
a_num_file::ParametricFile
a_num_file = (ParametricFile  "a" ["num"]  )
b_num_file = (ParametricFile  "b" ["num"] )
c_file = (ParametricFile "c" [])

rule1::ParametricRule
rule1 = ParametricRule "sort" [a_num_file]  b_num_file

num_params i = [(Param "num" (SValue (show i)))]

bs = [ anchor_file (num_params num) b_num_file | num <- [1..4]]
rule2::AnchoredRule
rule2 = AnchoredRule  "cat" bs   (AnchoredParametricFile c_file [])

anchor_file::Params->ParametricFile->AnchoredParametricFile
anchor_file params pf  = (AnchoredParametricFile pf params)



-- can this parametric_rule generate file with name,params?
-- yes, give back the rule or nothing
-- output
anchor_parameter::ParametricRule->Name->Params->Maybe AnchoredRule
anchor_parameter pr name params =
    let (ParametricRule cmd inputs output) = pr in
    let (ParametricFile oname oparam_names) =  output in 
    if oname /= name then Nothing
    else if (not (are_params_covered oparam_names params)) then Nothing else
 --   data ParametricFile = ParametricFile Name ParamNames FileGen 
 --data Rule = Rule Command [AnchoredParametricFile] AnchoredParametricFile  deriving (Show)
    Just (AnchoredRule cmd (map (anchor_file params) inputs) (anchor_file params output))




params = [(Param "num" (SValue "7"))]
main = do
    print (rule1)
 --   print (tree )
 --   print (execution $ select $ tree)
    print "hello"