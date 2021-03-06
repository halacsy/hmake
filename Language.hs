{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Language where
import Data.Maybe
import Data.List.Split
import Schema
import Control.Monad.Instances
import Control.Monad
import qualified Prelude 
import Prelude hiding (filter, sum)
import Data.List (deleteBy)


type FileName = String
data File = UnixFile FileName  | PigFile FileName deriving (Show, Eq)

nameOfFile::File->FileName
nameOfFile (PigFile name) = name

data Node = 
  InputFile Schema File |
  Transformer (Maybe File) Schema PipeCmd |

  TaskGroup [Node] deriving (Show, Eq)

typeOfNode::Node->Schema
typeOfNode (InputFile schema _) = schema 
typeOfNode (Transformer _ schema _) = schema
typeOfNode _ = undefined


storedAsHdfs::Either String Node->String->Either String Node
storedAsHdfs l@(Left _) _ = l
storedAsHdfs (Right (Transformer Nothing schema pipecmd)) o = 
          Right $ Transformer (Just (PigFile o)) schema pipecmd

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
schemaOfNode (Transformer _ s p) = s
{-
outputOfNode::Node->Maybe File
outputOfNode (InputFile _ f) = Just f
outputOfNode (Transformer  f _)  =  f
outputOfNode (TaskGroup _) = Nothing

getOutputFiles::[Node]->[File]
getOutputFiles nodes = map fromJust $ Prelude.filter isJust $ map outputOfNode nodes

-}
getActualFile (UnixFile f) = f
getActualFile (PigFile f) = "/mnt/hdfs" ++ f



data Exp = IA Int | SA String | Sub Exp Exp | Selector Selector | Sum Selector | Count Selector | SubString Exp Int Int deriving (Show, Eq)

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

-- TODO: doesn't work for complex selectors
instance Selectorr String where
  toSelector ('$':col) = Name col -- Pos $ ( (read col)::Int)
  toSelector name = case (splitOn "." name) of
                      [] -> Name name
                      (x:[]) -> Name name
                      (x1:x2:[]) -> ComplexSelector (toSelector x1) (toSelector x2)

instance Selectorr Selector where
  toSelector = id

data ComparisonOperator = Eq | Neq | Lt | Gt | LtE | GtE | Matches deriving (Show, Eq)

data Condition = Comp ComparisonOperator Exp Exp | And Condition Condition | Or Condition Condition deriving (Show, Eq)

data GeneratingExp = Flatten Selector | Exp Exp (Maybe String) | NestedProjection Selector [Selector] deriving (Show, Eq)
data PipeCmd = Generate [GeneratingExp] Node 
               | GroupBy [Exp] Node 
               | Filter Condition Node 
               | Distinct Node 
               | Union [Node]
               | Join JoinType Node Selector Node Selector
             --  | Load String 
               deriving (Show, Eq)

data JoinType = Inner | LeftOuter | RightOuter | FullOuter deriving (Show, Eq)

getDependencies::PipeCmd->[Node]
getDependencies (Union nodes) = nodes
getDependencies (Generate _ p) = [p]
getDependencies (GroupBy _ p) = [p] 
getDependencies (Filter _ p) = [p]
getDependencies (Distinct p) = [p]
getDependencies (Join _ pipe1 _ pipe2 _) = [pipe1, pipe2]


instance Expp Int where
  toExp = IA 

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

typeOf::Exp->Schema->Either String NamedT
typeOf (IA _) _ = Right (Nothing, I)
typeOf (SA _) _ = Right (Nothing, S)
typeOf (SubString exp i j) context = if j < i then Left "invalid start stop"
                                     else 
                                      case typeOf exp context of
                                      Right (_ , S) -> Right  (Nothing, S)
                                      Left s -> Left s
                                      otherwise -> Left "substring of a non string"

typeOf (Sum selector) context = do
                            t <- typeOf (Selector selector) context
                            case t of
                              (Just n, L) -> return (Just $ "sumOf" ++ n, L)
                              (Just n, I) -> return (Just $ "sumOf" ++ n, L)
                              (Nothing, _) -> fail $ "can't sum noname cols"
                              otherwise -> fail $ "can't sum non integral type " ++ (show t) ++ " in " ++ (show context)
typeOf (Count selector) context = -- exr must point to a BAG
                            do
                                t <- typeOf (Selector selector) context 
                                case t of
                                    (_, B _) -> return (Nothing, L)
                                    otherwise -> fail $ "COUNT doesn't refer to a bag with " ++ (show selector) 
typeOf (Sub exp1 exp2) context = do
     (_, t1) <- typeOf exp1 context
     (_, t2) <- typeOf exp2 context
     if t1 == t2 then return (Nothing, t1) else fail $ "uncombatible types" ++ (show  exp1) ++ " " ++  (show exp2) 

typeOf (Selector (Pos x) ) context =
                                   if x > length context - 1 
                                     then fail $ "out of index " ++ (show x) ++ " in " ++ show context
                                   else
                                     return (context !! x)

typeOf (Selector (Name n)) context = case lookup (Just n) context of  
                                        Just v -> return (Just n, v)
                                        Nothing -> fail $ "don't find: " ++ n ++ " in " ++ (show context)

typeOf (Selector (ComplexSelector sel1 sel2)) context  = do
    context' <- typeOf (Selector sel1) context
    case context' of
         (_, T schema) -> typeOf (Selector sel2) schema
         (_, B schema) -> typeOf (Selector sel2) schema
         otherwise ->  fail $ "can't subselect with" ++ show sel1 ++ show sel2 ++ " in " ++ show context

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

matches:: (Expp a) => a -> String -> Condition
matches a b = Comp Matches (toExp a) (SA b)

elem :: (Expp a, Expp b) => a -> [b] -> Condition
elem x' s' = opJoin Or expressions 
    where
        x = toExp x'
        s = map toExp s'
        expressions = map (eq x) s
        opJoin o (x:[]) = x
        opJoin o (x:xs) = o x $ opJoin o xs
        
type Step = Node->Either String Node
cut :: (Selectorr a) => [(a, String)] -> Step
cut selectors  = select (map (\(s, name) -> Exp (toExp (toSelector s)) (Just name)) selectors)

select::[GeneratingExp]->Step
select xs prev = do
        types <- mapM typeOfGenExp xs
        let gen_type = concat types
        return $ Transformer Nothing  gen_type (Generate xs prev)
        where
            context = schemaOfNode prev
            typeOfGenExp::GeneratingExp->Either String [NamedT]
            typeOfGenExp (Exp e (Just name)) = sequence [rename name $ typeOf e context]
            typeOfGenExp (Exp e Nothing) = sequence [ typeOf e context]
            typeOfGenExp (NestedProjection sel1 sels) = mapM (\sel -> typeOf (Selector $ ComplexSelector sel1 sel) context) sels >>= (\schema -> Right [(Just "elements", B schema)]) 
            typeOfGenExp (Flatten selector) = case typeOf (Selector selector) context of
                    Left s -> Left s 
                    Right (_, T schema) -> Right schema
                    Right (_, B _) -> Left $ "can't flatten bags"
                    Right x -> Right [x]
 
groupBy::[Exp]-> Step
groupBy xs p = 
        case gen_type of
          Left s -> Left s
          Right t -> Right $ Transformer Nothing t $ GroupBy xs p
        where
            gen_type::Either String [NamedT]
            gen_type =  myConcat groupType  groupValueType
 
            groupType::Either String [NamedT]
            groupType = if length xs == 1 then 
                          do
                            (name, x) <- typeOf (head xs) (schemaOfNode p)
                            return [(name, x)]
                        else 
                            -- if there are more elements pig creates a tuple
                            do
                                x <- sequence $ map (\t -> typeOf t (schemaOfNode p)) xs
                                return [(Just "group", T x )]

            groupValueType::Either String [NamedT]
            groupValueType = let orig_typ = schemaOfNode p in 
                             Right [(Just "elements", B orig_typ)]



comparableTypes::NamedT->NamedT->Bool
comparableTypes (_, typ1) (_, typ2) =  -- we are very strict here. No casting accepted! 
            typ1 == typ2

typeOfCondition::Condition->Node->Either String [NamedT]
typeOfCondition (Comp operator exp1 exp2) pipe = do
            type1 <- typeOf exp1 (schemaOfNode pipe)
            type2 <- typeOf exp2 (schemaOfNode pipe)
            -- we are very strict here. No casting accepted!
            if comparableTypes type1 type2 then 
              return [(Nothing, Bool)] 
            else 
              fail $ "can't compare " ++ (show type1) ++ " to " ++ (show type2)

typeOfCondition (Or exp1 exp2) pipe = Right [(Nothing, Bool)]

filter::Condition->Node->Either String Node
filter cond prev = do
                  -- it fails if not works
                  _ <- typeOfCondition cond prev
                  Right (Transformer Nothing  (typeOfNode prev) (Filter cond prev))


distinct:: Step
distinct prev = Right $ Transformer Nothing (typeOfNode prev) (Distinct prev)
{- old style should be simple InputFile 
load::File->Schema->PipeE
load s t = Right (t, Load s)

-}

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
          return $ Transformer Nothing schema $ Union nodes
 
--(>>>)::Transformer->Transformer->Transformer
x >>> y = \p -> (Right p) >>= x >>= y
    
{- 
user_shows = GROUP relevant_shows BY user_id;
user_show_count = FOREACH user_shows GENERATE group as user_id,
COUNT(relevant_shows.user_id) as showcount;
-}


freq::(Selectorr s) => [s]->Step
freq col = groupBy (map (toExp . toSelector) col) >>> select [Flatten $ Pos 0, Exp ( Count  (Pos 1)) (Just "count") ]

pushDown::Selector->Exp->Exp
pushDown sp (Sum sc) = Sum (ComplexSelector sp sc)

--group::(Selectorr s) => [s]->Exp->Step
group col fun = groupBy (map (toExp . toSelector) col)  >>> select [Flatten $ Pos 0, Exp ( pushDown (Pos 1) fun ) (Just "count") ]

sum :: Selectorr a => a -> Exp
sum s = Sum (toSelector s)

substring:: Selectorr a => a-> Int->Int->Exp
substring s start stop = SubString (toExp $ toSelector s) start stop
{-}
cum col = group col (Count  (Pos 1))
-}
{- still old 
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
-}
(<:>)::String->Exp->(Exp, String)
n <:> e = (e, n)

join = undefined
{- old style 
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

-}

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