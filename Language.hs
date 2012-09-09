{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Language where
import Data.Maybe

import Schema
import Control.Monad.Instances
import Control.Monad
import qualified Prelude 
import Prelude hiding (filter)
import Data.List (deleteBy)


type FileName = String
data File = UnixFile FileName  | PigFile FileName deriving (Show, Eq)

nameOfFile::File->FileName
nameOfFile (PigFile name) = name

data Node = 
  InputFile Schema File |
  Transformer (Maybe File) Pipe |

  TaskGroup [Node] deriving (Show, Eq)

pig_node::Either String Pipe->String->Either String Node
pig_node (Left s) _ = Left s
pig_node (Right pipe) o = -- we need to find the dependencies
        Right $ Transformer (Just (PigFile o)) pipe


storedAsHdfs::Either String Node->String->Either String Node
storedAsHdfs l@(Left _) _ = l
storedAsHdfs (Right node) o = Right $ Transformer (Just (PigFile o)) (schemaOfNode node, Node node)

data Dependency = All [Node] | Any [Node] deriving (Show)

doAllOf::[Either String Node] -> Either String Node
doAllOf nodes = do
    nodes' <- sequence nodes
    return $ TaskGroup nodes'
    

source::Dependency->[Node]
source (All nodes) = nodes
source (Any nodes) = nodes


-- later node can inherit schema
schemaOfNode::Node->Schema
schemaOfNode (InputFile s _) = s
schemaOfNode (Transformer _ (s, p)) = s

outputOfNode::Node->Maybe File
outputOfNode (InputFile _ f) = Just f
outputOfNode (Transformer  f _)  =  f
outputOfNode (TaskGroup _) = Nothing

getOutputFiles::[Node]->[File]
getOutputFiles nodes = map fromJust $ Prelude.filter isJust $ map outputOfNode nodes


getActualFile (UnixFile f) = f
getActualFile (PigFile f) = "/mnt/hdfs" ++ f



data Exp = IA Int | SA String | Sub Exp Exp | Selector Selector | Sum Selector | Count Selector  deriving (Show, Eq)

-- TODO rename to Exp
class Expp a where
  toExp::a -> Exp



data Selector = Pos Int | Name String | ComplexSelector Selector Selector deriving (Show, Eq)

instance Expp Selector where
  toExp s = (Selector s)

instance Expp Exp where
  toExp = id

class Selectorr a where
  toSelector::a->Selector

instance Selectorr String where
  toSelector ('$':col) = undefined
  toSelector name = Name name 

data ComparisonOperator = Eq | Neq | Lt | Gt | LtE | GtE | Matches deriving (Show, Eq)

data Condition = Comp ComparisonOperator Exp Exp | And Condition Condition | Or Condition Condition deriving (Show, Eq)

type Pipe = (Schema,PipeCmd)

class Feedable a where
  toPipe :: a -> Pipe
         
instance Feedable Node where
  toPipe node@(InputFile schema _ ) = (schema, Node node)
  toPipe node@(Transformer _ (s, p) ) = (s, Node node)
 

instance Feedable Pipe where
  toPipe p = p
{- 
instance Feedable (Pipe) where
  toPipe  = id
-}
-- we need a better name for this, PipeE. Maybe Pipe must be hided and PipeE can be public
type PipeE = Either String Pipe 

data GeneratingExp = Flatten Selector | Exp Exp (Maybe String) | NestedProjection Selector [Selector] deriving (Show, Eq)
data PipeCmd = Generate [GeneratingExp] Pipe 
               | GroupBy [Exp] Pipe 
               | Filter Condition Pipe 
               | Distinct Pipe 
               | Union [Node]
               | Node Node
               | Join JoinType Pipe Selector Pipe Selector
               | Load String deriving (Show, Eq)

data JoinType = Inner | LeftOuter | RightOuter | FullOuter deriving (Show, Eq)

-- this is not a final design, and doesn't represent what we want to achive
data SortOut = SortOut Pipe [(Condition, File)]

getDependencies::PipeCmd->[Node]
getDependencies (Node node) = [node]
getDependencies (Union nodes) = nodes
getDependencies (Generate _ p) = getPipeDependencies p
getDependencies (GroupBy _ p) = getPipeDependencies p
getDependencies (Filter _ p) = getPipeDependencies p
getDependencies (Distinct p) = getPipeDependencies p
getDependencies (Load _ ) = []
getDependencies (Join _ pipe1 _ pipe2 _) = (getPipeDependencies pipe1) ++ (getPipeDependencies pipe2)

getPipeDependencies (_, p) = getDependencies p

instance Expp Int where
  toExp = IA 

schemaOfPipe::Pipe->Schema
schemaOfPipe (s, _) = s

{-
instance Monad (Either String) where
  return v = Right v
  fail s = Left s
  (Left s) >>= _ = Left s
  (Right v) >>= f = f v
-}

as::String->Schema
as str = [(Just "user_id", I), (Just "prezi_id", S), (Just "freq", I)] 


type PSchema = Either String Schema
myConcat::PSchema ->PSchema ->PSchema 
myConcat (Left s) _ = Left s
myConcat _ (Left s ) = Left s
myConcat (Right t1) (Right t2) = (Right (t1 ++t2)) 

typeOf::Exp->Pipe->Either String NamedT
typeOf (IA _) _ = Right (Nothing, I)
typeOf (SA _) _ = Right (Nothing, S)
typeOf (Sum selector) p = do
                            t <- typeOf (Selector selector) p
                            case t of
                              (Just n, L) -> return (Just $ "sumOf" ++ n, L)
                              (Just n, I) -> return (Just $ "sumOf" ++ n, L)
                              (Nothing, _) -> fail $ "can't sum noname cols"
                              otherwise -> fail $ "can't sum non integral type " ++ (show t) ++ " in " ++ (show p)
typeOf (Count selector) p = -- exr must point to a BAG
                            do
                                t <- typeOf (Selector selector) p 
                                case t of
                                    (_, B _) -> return (Nothing, L)
                                    otherwise -> fail $ "COUNT doesn't refer to a bag with " ++ (show selector) 
typeOf (Sub exp1 exp2) p = do
     (_, t1) <- typeOf exp1 p
     (_, t2) <- typeOf exp2 p
     if t1 == t2 then return (Nothing, t1) else fail $ "uncombatible types" ++ (show  exp1) ++ " " ++  (show exp2) 

typeOf (Selector (Pos x) ) (t, _) =  if x > length t - 1 
                                    then fail $ "out of index " ++ (show x) ++ " in " ++ show t
                                   else
                                    return (t !! x)

typeOf (Selector (Name n)) (t, p) = case lookup (Just n) t of  
                                        Just v -> return (Just n, v)
                                        Nothing -> fail $ "don't find: " ++ n ++ " in " ++ (show t) ++ (show p)

typeOf (Selector (ComplexSelector sel1 sel2)) (t, p) = do
    (_, typ) <- typeOf (Selector sel1) (t, p)
    case typ of
         T schema -> typeOf (Selector sel2) (schema, p)
         B schema -> typeOf (Selector sel2) (schema, p)
         otherwise ->  fail $ "can't subselect with" ++ show sel1 ++ show sel2 ++ " in " ++ show typ

rename::String->Either String NamedT->Either String NamedT
rename _ (Left s) = Left s
rename newName (Right (_, typ)) = Right (Just newName, typ)


p0 =  (Pos 0)
p1 =  (Pos 1)
p2 =  (Pos 2)
p3 =  (Pos 3)
p4 =  (Pos 4)

c::String->Selector
c name = (Name name)

eq :: (Expp a, Expp b) => a -> b -> Condition
eq a b = Comp Eq (toExp a) (toExp b)

elem :: (Expp a, Expp b) => a -> [b] -> Condition
elem x' s' = opJoin Or expressions 
    where
        x = toExp x'
        s = map toExp s'
        expressions = map (eq x) s
        opJoin o (x:[]) = x
        opJoin o (x:xs) = o x $ opJoin o xs
        
cut :: (Selectorr a, Feedable f) => [(a, String)] -> f -> PipeE
cut selectors  = select (map (\(s, name) -> Exp (toExp (toSelector s)) (Just name)) selectors)

select::Feedable a =>[GeneratingExp]->a->PipeE
select xs prev = do
        types <- mapM typeOfGenExp xs
        let gen_type = concat types
        return (gen_type, Generate xs p)
        where
            p = toPipe prev
            typeOfGenExp::GeneratingExp->Either String [NamedT]
            typeOfGenExp (Exp e (Just name)) = sequence [rename name $ typeOf e p]
            typeOfGenExp (Exp e Nothing) = sequence [ typeOf e p]
            typeOfGenExp (NestedProjection sel1 sels) = mapM (\sel -> typeOf (Selector $ ComplexSelector sel1 sel) p) sels >>= (\schema -> Right [(Just "elements", B schema)]) 
            typeOfGenExp (Flatten selector) = case typeOf (Selector selector) p of
                    Left s -> Left s 
                    Right (_, T schema) -> Right schema


groupBy::Feedable a => [Exp]-> a -> PipeE
groupBy xs prev = 
        case gen_type of
          Left s -> Left s
          Right t -> Right (t, GroupBy xs p)
        where
            p = toPipe prev 
            gen_type::Either String [NamedT]
            gen_type =  myConcat groupType  groupValueType
 
            groupType::Either String [NamedT]
            groupType = if length xs == 1 then 
                          do
                            (_, x) <- typeOf (head xs) p
                            return [(Just "group", x)]
                        else 
                            -- if there are more elements pig creates a tuple
                            do
                                x <- sequence $ map (\t -> typeOf t p) xs
                                return [(Just "group", T x )]

            groupValueType::Either String [NamedT]
            groupValueType = let (orig_typ, _) = p in 
                             Right [(Just "elements", B orig_typ)]


comparableTypes::NamedT->NamedT->Bool
comparableTypes (_, typ1) (_, typ2) =  -- we are very strict here. No casting accepted! 
            typ1 == typ2

typeOfCondition::Condition->Pipe->Either String [NamedT]
typeOfCondition (Comp operator exp1 exp2) pipe = do
            type1 <- typeOf exp1 pipe
            type2 <- typeOf exp2 pipe
            -- we are very strict here. No casting accepted!
            if comparableTypes type1 type2 then 
              return [(Nothing, Bool)] 
            else 
              fail $ "can't compare " ++ (show type1) ++ " to " ++ (show type2)

typeOfCondition (Or exp1 exp2) pipe = Right [(Nothing, Bool)]

filter::Condition->Node->Either String Node
filter cond prev = do
                  let p@(t, _) = (schemaOfNode prev, Node prev)
                  _ <- typeOfCondition cond p
                  Right (Transformer Nothing $ (t, Filter cond p))


distinct::Feedable a => a -> PipeE
distinct prev = let p@(t, _ ) = toPipe prev in Right (t, (Distinct p))

load::String->Schema->PipeE
load s t = Right (t, Load s)

input::Either String Node->PipeE
input (Left s) = Left s
input (Right  node@(InputFile schema _ )) = Right (schema, Node node)
input (Right node@(Transformer  _ (schema,p) )) = Right (schema, Node node)

{-
union::Feedable a => [Either String a] -> PipeE
union [] = Left "empty union"
union (x:[]) = Right (toPipe x)
union xs = Right (schema, Union p)
      where
        schema = schemaOfPipe (head p)
        p  sequence $ map toPipe xs
-}
union:: [Either String Node]->Either String Node
union [] = Left "empty Union"
union ((Right x):[]) = Right x
union ((Left s):[]) = Left s
union xs = -- TODO check scheme equality
      do 
          nodes <- sequence xs
        
          let schema = schemaOfNode (head nodes)
          return (Transformer Nothing (schema, Union nodes))
 
--(>>>)::Transformer->Transformer->Transformer
x >>> y = \p -> (Right p) >>= x >>= y
    
{- 
user_shows = GROUP relevant_shows BY user_id;
user_show_count = FOREACH user_shows GENERATE group as user_id,
COUNT(relevant_shows.user_id) as showcount;
-}


freq::(Selectorr s, Feedable a) => [s]->a -> PipeE
freq col = groupBy (map (toExp . toSelector) col) >>> select [Flatten $ Pos 0, Exp ( Count  (Pos 1)) (Just "count") ]

--group::(Selectorr s, Feedable a) => [s]->a -> PipeE
group col fun = groupBy (map (toExp . toSelector) col) >>> select [Flatten $ Pos 0, Exp ( fun ) (Just "count") ]

cum col = group col (Count  (Pos 1))

groupByCol::(Selectorr s, Feedable a) => s -> a -> PipeE
groupByCol col f = do
    let pipe@(schema, _) = toPipe f
    let ex = toExp $ toSelector col
    (name, typ) <- typeOf ex pipe
    groupped <- groupBy [ex] pipe
    let ranked = zip [0..] schema
    let eqq a b = (fst ( snd a) ) ==  (fst ( snd b ))
    let newRankedSchema = deleteBy eqq (-1, (name, typ)) ranked
    let newCols = NestedProjection (Pos 1) $ map (\(r, (n, _))-> Pos r) newRankedSchema
    select [(Exp (Selector $ Pos 0) name) , newCols] groupped

(<:>)::String->Exp->(Exp, String)
n <:> e = (e, n)

join :: (Feedable a, Feedable b) => Either String a -> Either String b -> String -> PipeE
join (Left s) _  _ = Left s
join _  (Left s) _ = Left s

join (Right f1)  (Right f2) name = do
          let pipe1@(schema1, cmd1) = toPipe f1
          let pipe2@(schema2, cmd2) = toPipe f2
          typ1 <- typeOf (Selector $ Name name) pipe1
          typ2 <- typeOf (Selector $ Name name) pipe2
          if typ1 /= typ2 then fail $ "can't join by incompatible " ++ name ++ show typ1 ++ " and " ++ name ++ show typ2
          else
            return ([(Just "NOT", I)], Join FullOuter pipe1 (Name name) pipe2  (Name name))



{-

a::Either String Pipe
a =  load "vacak" [(Just "user_id", I), (Just "prezi_id", S), (Just "freq", I)] 
      >>= generate [Selector (Pos 1), Selector (Name "freq")] 
      >>= filter (elem p1 [1, 2, 5])
      >>= groupBy [Selector (Pos 1)] 
      >>= distinct


main = do
    case a of
        Left s -> print s
        Right p -> print p

   -}     