module Pig.Dsl 
(
    group, filter, cut, freq, (->>), load, store, elem, eq, distinct, PFilter
)
where
import Pig.Language 
import Prelude hiding (filter, elem)



type PFilter = PigExpr->PigExpr


load::String->PigExpr
load s =  Load s Nothing

store::String->PFilter
store s = Store s

--(.) :: (b -> c) -> (a -> b) -> a -> c
-- (a -> b) -> a -> b
(->>)::PFilter->PFilter->PFilter
(->>) f g = g . f



g::Int
g= 5

group::Expr->PFilter
group e = Group e

filter::Expr->PFilter
filter e = Filter e

cut::[Int]->PFilter
cut cols = Foreach selectors 
    where
        selectors = map Positional cols

{- 
user_shows = GROUP relevant_shows BY user_id;
user_show_count = FOREACH user_shows GENERATE group as user_id,
COUNT(relevant_shows.user_id) as showcount;
-}

freq::[Int]->PFilter
freq col sub = Foreach [Positional 0, (Count (Positional 1))] groupped
    where
        groupped = Group (Tuple (map Positional col)) sub

{-
aw_users = FOREACH rows_with_users GENERATE p1 as user_id; 

users = DISTINCT raw_users; 

-}
distinct::[Int]->PFilter
-- why do i have to write x here?
distinct x = Distinct . cut x

select:: PigValue a => Int-> ComparisonOperator -> a->PigExpr ->PigExpr
select i op v = filter $ CompExpr op (Positional i) (toPValue v)

eq :: PigValue a => Int -> a -> PFilter
eq i v = select i Eq v

elem :: PigValue a => Int -> [a] -> PFilter
elem i l = filter $ opJoin Or expressions 
    where
        expressions = map (\v -> CompExpr Eq (Positional i) (toPValue v)) l
        opJoin::BoolOperator->[Expr]->Expr
        opJoin o (x:[]) = x
        opJoin o (x:xs) = BoolExpr o x $ opJoin o xs
        
