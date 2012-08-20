{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Language where
import Data.Maybe
import Control.Monad.Instances
import Prelude hiding (filter)
data Exp = IA Int | SA String | Sub Exp Exp | Selector Selector | Sum Selector | Count Selector deriving (Show, Eq)


data Selector = Pos Int | Name String deriving (Show, Eq)

data ComparisonOperator = Eq | Neq | Lt | Gt | LtE | GtE | Matches deriving (Show, Eq)

data Condition = Comp ComparisonOperator Exp Exp | And Condition Condition | Or Condition Condition deriving (Show, Eq)



data Typ = I | S | L | Bool | T Schema | B Schema deriving (Show, Eq)
type Name = Maybe String
type NamedT = (Name, Typ)
type Schema = [NamedT]

type Transformer = Pipe->Either String Pipe

data PipeCmd = Generate [Exp] Pipe | GroupBy [Exp] Pipe | Filter Condition Pipe | Load String deriving (Show)

type Pipe = (Schema,PipeCmd)

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

generate::[Exp]->Transformer
generate xs p = case gen_type of 
        Left s -> Left s
        Right t ->  Right (t, Generate xs p)
        where
            gen_type::Either String [NamedT]
            gen_type =  sequence $ map (\t -> typeOf t p) xs

groupBy::[Exp]->Transformer
groupBy xs p = case gen_type of
        Left s -> Left s
        Right t -> Right (t, GroupBy xs p)
        where
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

typeOfCondition::Condition->Pipe->Either String [NamedT]
typeOfCondition (Comp operator exp1 exp2) pipe = do
            type1 <- typeOf exp1 pipe
            type2 <- typeOf exp2 pipe
            -- we are very strict here. No casting accepted!
            if type1 == type2 then return [(Nothing, Bool)] else fail $ "can't compare " ++ (show type1) ++ " to " ++ (show type2)

filter::Condition->Transformer
filter cond p@(t, _) = do
                  _ <- typeOfCondition cond p
                  Right (t, Filter cond p)

load::String->Schema->Either String Pipe
load s t = Right (t, Load s)
{-}
a::Either String Pipe
a =  load "vacak" [(Just "user_id", I), (Just "prezi_id", S), (Just "freq", I)] 
      >>= generate [Selector (Pos 1), Selector (Name "freq")] 
      >>= filter (Comp Eq (Selector (Pos 1)) (Selector (Pos 2)))
      >>= groupBy [Selector (Pos 1)] 
      >>= generate [Count (Pos 1)]


main = do
    case a of
        Left s -> print s
        Right p -> print p
-}