{- NoMonomorphismRestriction -}
import Language
import PigCmd
import Text.Printf
import Util
import Graph 
import Cli
import Prelude hiding (elem, filter)
kpiCodesWithUserActivity::[Int]
kpiCodesWithUserActivity = [0,1, 2,3,4,5,6,10,11,13, 14, 17,19, 20, 21, 30]

type DaylyFile = Int->Int->Int->Either String Node

kpi_log y m d = Right $ InputFile (as "user_id:int, prezi:id") (PigFile  $ printf "/scribe/kpi/kpi-%04d-%02d-%02d_00000" y m d)



kpi_code = 4
kpi_user = 5
kpi_prezi = 6

--base = "/user/hp/"
base = "/Users/hp/"
out_base name = base ++ name ++ "-%04d-%02d-%02d"


daily_uniq_users ::DaylyFile
daily_uniq_users  y m d = pig (  filter (Comp Eq (Selector (Pos kpi_code)) (IA 3))  )  
                              [kpi_log y m d] 
                              (printf (out_base "daily_uniq_users") y m d )

vacak y m d = pig (filter (Comp Eq (Selector (Pos 5)) (IA 3)))
						[daily_uniq_users y m d]
						(printf (out_base "vacak") y m d )                       
{-
monthly_uniq_users::Int->Int->Node
monthly_uniq_users y m = pig (distinct [0])
                             [daily_uniq_users y m d | d <- days_of_month y m] 
                             (printf (out_base "monthly_uniq_users") y m  )


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
  doIt $ vacak 2012 05 27


    
