{- NoMonomorphismRestriction -}
import Language
import PigCmd
import Text.Printf
import Util
import Graph 
import Cli
import Prelude hiding (elem, filter)

kpiCodesWithUserActivity = [0,1, 2,3,4,5,6,10,11,13, 14, 17,19, 20, 21, 30]

type DaylyFile = Int->Int->Int->Either String Node
type MonthlyFile = Int -> Int -> Either String Node

kpi_schema = [(Just "date", S), 
              (Just "time", S), 
              (Just "hostname", S), 
              (Just "logtype", S), 
              (Just "type", I), 
              (Just "p1", I), 
              (Just "p2", S), 
              (Just "p3", S)] 

--	kpi_log y m d = Right $ InputFile kpi_schema (PigFile  $ printf "/scribe/kpi/kpi-%04d-%02d-%02d_00000" y m d)
kpi_log y m d = Right $ InputFile kpi_schema (PigFile  "/Users/hp/Documents/HasMake/vacak.log")



kpi_code = 4
kpi_user = 5
kpi_prezi = 6

--base = "/user/hp/"
base = "/Users/hp/"
out_base name = base ++ name ++ "-%04d-%02d-%02d"


daily_uniq_users ::DaylyFile
daily_uniq_users  y m d = pig ( filter ( c "type" `elem` kpiCodesWithUserActivity )  >>> select [(c "p1", "user_id")] >>> distinct )  
                              [kpi_log y m d] 
                              (printf (out_base "daily_uniq_users") y m d )

                       

monthly_uniq_users::MonthlyFile
monthly_uniq_users y m = pig (distinct )
                             [daily_uniq_users y m d | d <- days_of_month y m] 
                             (printf (out_base "monthly_uniq_users") y m  )

{-}
daily_user_prezi_edit_freqs::DaylyFile
daily_user_prezi_edit_freqs y m d = pig (eq kpi_code (3::Int) ->> freq [kpi_user, kpi_prezi]) 
                              [vacak y m d] 
                              (printf (out_base "daily_user_prezi_edit_freqs") y m d )


daily_user_save_counts::DaylyFile
daily_user_save_counts y m d = pig (sum_by 2 [0]) 
                               [daily_user_prezi_edit_freqs y m d]
                               (printf (out_base "daily_user_save_counts" ) y m d )
-}
main = do
  doIt $ monthly_uniq_users 2012 05 


    
