module Pig.Dsl 
(
	group, filter, cut, freq, select, (->>), load, store, PFilter
)
where
import Pig.Language	
import Prelude hiding (filter)



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
cut cols e = Foreach e tup 
	where
		tup = Tuple selectors
		selectors = map Positional cols

{- 
user_shows = GROUP relevant_shows BY user_id;
user_show_count = FOREACH user_shows GENERATE group as user_id,
COUNT(relevant_shows.user_id) as showcount;
-}

freq::Int->PFilter
freq col sub = Foreach groupped (Tuple [Positional 0, (Count (Positional 1))]) 
	where
		groupped = Group (Positional col) sub

select:: PigValue a => Int-> ComparisonOperator -> a->PigExpr ->PigExpr
select i op v = filter $ BoolExpr op (Positional i) (toPValue v)

