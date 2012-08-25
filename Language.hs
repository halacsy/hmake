{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Language where
import Data.Maybe
import Graph
import Schema
import Control.Monad.Instances
import Prelude hiding (filter, elem)

data Exp = IA Int | SA String | Sub Exp Exp | Selector Selector | Sum Selector | Count Selector | Flatten Selector deriving (Show, Eq)


data Selector = Pos Int | Name String deriving (Show, Eq)

data ComparisonOperator = Eq | Neq | Lt | Gt | LtE | GtE | Matches deriving (Show, Eq)

data Condition = Comp ComparisonOperator Exp Exp | And Condition Condition | Or Condition Condition deriving (Show, Eq)

type Pipe = (Schema,PipeCmd)

class Feedable a where
  toPipe :: a -> Pipe
         
instance Feedable Node where
  toPipe node@(InputFile schema _ ) = (schema, Node node)
  toPipe node@(FileGenerator schema _ _ _ ) = (schema, Node node)
 

instance Feedable Pipe where
  toPipe p = p
{- 
instance Feedable (Pipe) where
  toPipe  = id
-}
-- we need a better name for this, PipeE. Maybe Pipe must be hided and PipeE can be public
type PipeE = Either String Pipe 


data PipeCmd = Generate [(Exp, String)] Pipe 
               | GroupBy [Exp] Pipe 
               | Filter Condition Pipe 
               | Distinct Pipe 
               | Union [Pipe]
               | Node Node
               | Merge Pipe Pipe 
               | Load String deriving (Show)

-- this is not a final design, and doesn't represent what we want to achive
data SortOut = SortOut Pipe [(Condition, File)]

getDependencies::PipeCmd->[Node]
getDependencies (Node node) = [node]
getDependencies (Union ps) = concatMap getPipeDependencies ps
getDependencies (Generate _ p) = getPipeDependencies p
getDependencies (GroupBy _ p) = getPipeDependencies p
getDependencies (Filter _ p) = getPipeDependencies p
getDependencies (Distinct p) = getPipeDependencies p
getDependencies (Load _ ) = []

getPipeDependencies (_, p) = getDependencies p

instance Num Exp where
  (+) = undefined
  (*) = undefined
  abs = undefined
  signum = undefined
  fromInteger i = IA  (fromInteger i) 

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
                                    then fail $ "out of index " ++ (show x) 
                                   else
                                    return (t !! x)

typeOf (Selector (Name n)) (t, _) = case lookup (Just n) t of  
                                        Just v -> return (Just n, v)
                                        Nothing -> fail $ "don't find: " ++ n


rename::String->Either String NamedT->Either String NamedT
rename _ (Left s) = Left s
rename newName (Right (_, typ)) = Right (Just newName, typ)


p0 = Selector (Pos 0)
p1 = Selector (Pos 1)
p2 = Selector (Pos 2)
p3 = Selector (Pos 3)
p4 = Selector (Pos 4)

c::String->Exp
c name = Selector (Name name)

eq::Exp->Exp->Condition
eq = Comp Eq

elem :: Exp -> [Exp] -> Condition
elem x s = opJoin Or expressions 
    where
        expressions = map (eq x) s
        opJoin o (x:[]) = x
        opJoin o (x:xs) = o x $ opJoin o xs
        


select::Feedable a =>[(Exp, String)]->a->PipeE
select xs prev = case gen_type of 
        Left s -> Left s
        Right t ->  Right (t, Generate xs p)
        where
            p = toPipe prev
            gen_type::Either String [NamedT]
            gen_type =  sequence $ map renameType xs
            renameType = \(exp, name) -> rename name $ typeOf exp p

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

filter::Feedable a =>Condition->a->PipeE
filter cond prev = do
                  let p@(t, _) = toPipe prev
                  _ <- typeOfCondition cond p
                  Right (t, Filter cond p)


distinct::Feedable a => a -> PipeE
distinct prev = let p@(t, _ ) = toPipe prev in Right (t, (Distinct p))

load::String->Schema->PipeE
load s t = Right (t, Load s)

input::Either String Node->PipeE
input (Left s) = Left s
input (Right  node@(InputFile schema _ )) = Right (schema, Node node)
input (Right node@(FileGenerator schema _ _ _ )) = Right (schema, Node node)

{-
union::Feedable a => [Either String a] -> PipeE
union [] = Left "empty union"
union (x:[]) = Right (toPipe x)
union xs = Right (schema, Union p)
      where
        schema = schemaOfPipe (head p)
        p  sequence $ map toPipe xs
-}
union::Feedable a => [Either String a]->PipeE
union [] = Left "empty Union"
union ((Right x):[]) = Right (toPipe x)
union ((Left s):[]) = Left s
union xs = -- TODO check scheme equality
      do 
          p <- sequence xs
          let p' = map toPipe p
          let schema = schemaOfPipe (head p')
          return (schema, Union p')
       
--(>>>)::Transformer->Transformer->Transformer
x >>> y = \p -> (Right p) >>= x >>= y
    
{- 
user_shows = GROUP relevant_shows BY user_id;
user_show_count = FOREACH user_shows GENERATE group as user_id,
COUNT(relevant_shows.user_id) as showcount;
-}


freq::Feedable a => [Selector]->a -> PipeE
freq col = groupBy (map Selector col) >>> select [(c "group", "group"), (Count  (Pos 1), "count") ]

sortOut::Feedable a => Selector->[(Exp, File)] -> a -> Either String SortOut
sortOut sel exps pipe = Right $ SortOut (toPipe pipe) conds
  where
    -- TODO: no verification here!
    conds = map (\(e, f) -> (eq (Selector sel) e, f)) exps

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