module Util (days_of_month)
where
import Data.Time.Calendar

--days_of_month y m = [1 .. gregorianMonthLength (fromIntegral y) m]
days_of_month y m = [1, 2]
{- 
main = do 
	print $ days_of_month 2012 2

-}
