{- NoMonomorphismRestriction -}
import Graph 
import Unix
import PigCmd 
import Pig.Dsl
import Pig.Language
import Prelude hiding (elem)
import Text.Printf

kpiCodesWithUserActivity::[Int]
kpiCodesWithUserActivity = [0,1, 2,3,4,5,6,10,11,13, 14, 17,19, 20, 21, 30]

kpi_log y m d = InputFile $ printf "/scribe/kpi/kpi-%04d-%02d-%02d_00000" y m d

daily_uniq_users ::Int->Int->Int->DepGraph
daily_uniq_users  y m d = pig ( elem 5 kpiCodesWithUserActivity ->> distinct [6] )  [kpi_log y m d] (printf "/user/hp/daily_uniq_users-%04d-%02d-%02d" y m d )

monthly_uniq_users::Int->Int->DepGraph
monthly_uniq_users y m = pig (distinct [1]) [daily_uniq_users y m d | d <- day_of_month y m] (printf "/user/hp/monthly_uniq_users-%04d-%02d" y m  )
day_of_month y m = [1..31]



main = do 
    g <- reduce $ monthly_uniq_users 2012 04
    g2 <- execution g
    mapM_  ( (execute False)) g2
    
