{- NoMonomorphismRestriction -}
import Pig.Dsl
import PigCmd
import Text.Printf
import Util
import Graph 
import Cli
import Prelude hiding (elem)
kpiCodesWithUserActivity::[Int]
kpiCodesWithUserActivity = [0,1, 2,3,4,5,6,10,11,13, 14, 17,19, 20, 21, 30]


kpi_log y m d = InputFile $ printf "/scribe/kpi/kpi-%04d-%02d-%02d_00000" y m d

vacak y m d = InputFile $ printf "/Users/hp/Documents/HasMake/vacak.log"

kpi_code = 4
kpi_user = 5
kpi_prezi = 6

daily_uniq_users ::Int->Int->Int->DepGraph
daily_uniq_users  y m d = pig ( elem 4 kpiCodesWithUserActivity ->> distinct [5] )  
                              [kpi_log y m d] 
                              (printf "/user/hp/daily_uniq_users-%04d-%02d-%02d" y m d )

monthly_uniq_users::Int->Int->DepGraph
monthly_uniq_users y m = pig (distinct [0])
                             [daily_uniq_users y m d | d <- days_of_month y m] 
                             (printf "/user/hp/monthly_uniq_users-%04d-%02d" y m  )


daily_prezi_edits::Int->Int->Int->DepGraph
daily_prezi_edits y m d = pig (eq kpi_code (3::Int) ->> freq [kpi_user, kpi_prezi]) 
                              [vacak y m d] 
                              (printf "/user/hp/daily_prezi_edits-%04d-%02d-%02d" y m d )


main = do
  doIt $ daily_prezi_edits 2012 05 28


    
