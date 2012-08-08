{- NoMonomorphismRestriction -}
import Graph 
import Unix
import PigCmd 
import Pig.Dsl
import Pig.Language

import Text.Printf

napi_log::Int->Int->Int->DepGraph
napi_log y m d = -- should be guards
    if (y, m, d) == (2012,4, 19) then
        InputFile $ printf "/Users/hp/Documents/Pig/log/kpi-%04d-%02d-%02d_00000" y m d 
    else
        cp [napi_log 2012 4 19] (printf "/Users/hp/Documents/Pig/log/kpi_gen-%04d-%02d-%02d_00000" y m d )

user y m d = (grep "3") [napi_log y m d]  (printf "/Users/hp/Documents/Pig/log/user-%04d-%02d-%02d_00000" y m d )
-- monthly_user y m  = uniq [user y m d | d <- day_of_month y m]  (printf "~/Documents/Pig/log/monthly-%04d-%02d" y m  )
t::Int
t= 3
monthly_user::Int->Int->DepGraph
monthly_user y m = pig (select 4 Eq (t::Int) ->> freq 5) [user y m d | d <- day_of_month y m] (printf "/Users/hp/Documents/Pig/log/monthly-%04d-%02d" y m  )
day_of_month y m = [1..31]



main = do 
    g <- reduce $ monthly_user 2012 04 
    g2 <- execution g
    mapM_  ( (execute True)) g2
    
