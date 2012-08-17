module Pig.Dsl 
(
    group_by, load, (->>)
    {- , filter, cut, freq, store, elem, eq, distinct, sum_by, PFilter -}
)
where
import Pig.Language 
import Prelude hiding (filter, elem)
import Pig.Types


type TypedExpr = (RelationT, PigExpr)

type PFilter = TypedExpr->TypedExpr


load::String->RelationT->TypedExpr
load file r = (r,  Load file Nothing)

store::String->TypedExpr->PigCommand
store file (t, e) = Store file e

--(.) :: (b -> c) -> (a -> b) -> a -> c
-- (a -> b) -> a -> b
(->>)::PFilter->PFilter->PFilter
(->>) f g = g . f




group_by::Expr->PFilter
group_by by (t, e) = (t', Group by e)
    where
        t' = [keyT, groupT]
        keyT = let (_, kt) = type_of_exp e t  in ("group", kt)
        groupT = BT $ TT t

{- 
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
freq col sub = Foreach [Flatten $ Positional 0, (Count (Positional 1))] groupped
    where
        groupped = Group (Tuple (map Positional col)) sub

sum_by::Int->[Int]->PFilter
sum_by x col sub = Foreach [Flatten $ Positional 0, (Sum (NestedPositional 1 1))] groupped
    where
        groupped = Group (Tuple (map Positional mapped_cols)) sub'
        sub' = cut (col ++ [x]) sub
        mapped_cols = [0.. (length col - 1)]

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
        
-}