module Util (days_of_month)
where
import Data.Time.Calendar

days_of_month y m = [1 .. gregorianMonthLength y m]

{- 
main = do 
	print $ days_of_month 2012 2

-}