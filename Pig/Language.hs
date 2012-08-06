{-# LANGUAGE TypeSynonymInstances #-}

module Pig.Language where

type Col = String

data Expr  =  IntExpr Int
           |  StringExpr String
           |  BoolExpr ComparisonOperator Expr Expr
           |  ArithExpr ArithmeticOperator Expr Expr
           |  Positional Int
           |  Tuple [Expr]
           |  Count Expr deriving (Show)


data ComparisonOperator = Eq | Neq | Lt | Gt | LtE | GtE | Matches deriving (Show)



data ArithmeticOperator = Add | Sub | Mul | Div | Mod | BinCond deriving (Show)


data PigExpr = Load String (Maybe Storage) | Group Expr PigExpr  | Filter Expr PigExpr | Foreach PigExpr Expr | Store String PigExpr deriving (Show)
		


data Storage = PigStorage deriving (Show)

type PError = String
class PigValue a where
	toPValue:: a -> Expr
	fromPValue::Expr -> Either PError a

instance PigValue Expr where
	toPValue = id
	fromPValue = Right

instance PigValue Int where
	toPValue = IntExpr
	fromPValue (IntExpr i) = Right i
	fromPValue _ = Left "not a pig int"

instance PigValue String where
	toPValue = StringExpr
	fromPValue (StringExpr s) = Right s
	fromPValue  _ = Left "not a pig string"

instance PigValue Integer where
	toPValue = IntExpr . fromIntegral
	fromPValue (IntExpr i) = Right $ fromIntegral i
	fromPValue _ = Left "not a pig int"
{- 
client_log = LOAD '/scribe/client/' USING PigStorage(' ');
show_kpi_rows = FILTER client_log BY $12 == 'show_kpi' ;
shows = FOREACH show_kpi_rows GENERATE $9 as user_id;
relevant_shows = FILTER shows BY user_id <= '13000000';

user_shows = GROUP relevant_shows BY user_id;
user_show_count = FOREACH user_shows GENERATE group as user_id,
COUNT(relevant_shows.user_id) as showcount;

one_time_showers = FILTER user_show_count BY showcount == 1;
one_time_userids = FOREACH one_time_showers GENERATE user_id;
-}





