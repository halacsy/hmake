{- NoMonomorphismRestriction -}
import Language
import PigCmd
import Text.Printf
import Util
import Graph 
import Cli
import Schema
import Prelude hiding (elem, filter)

kpiCodesWithUserActivity = [0,1, 2,3,4,5,6,10,11,13, 14, 17,19, 20, 21, 30]

type DaylyFile = Integer->Int->Int->Either String Node
type MonthlyFile = Integer -> Int -> Either String Node

kpi_schema = [(Just "date", S), 
              (Just "time", S), 
              (Just "hostname", S), 
              (Just "logtype", S), 
              (Just "type", I), 
              (Just "p1", I), 
              (Just "p2", S), 
              (Just "p3", S)] 

--	kpi_log y m d = Right $ InputFile kpi_schema (PigFile  $ printf "/scribe/kpi/kpi-%04d-%02d-%02d_00000" y m d)
kpi_log_raw::Integer->Int->Int->Either String Node
kpi_log_raw y m d = Right $ InputFile kpi_schema (PigFile  $"/Users/hp/Documents/HasMake/vacak" ++ show d ++ ".log")



kpi_code = 4
kpi_user = 5
kpi_prezi = 6

--base = "/user/hp/"
base = "/Users/hp/"
out_base name = base ++ name 


daily_uniq_users ::DaylyFile
daily_uniq_users  y m d = pig_node (  kpi_log_raw y m d
                                      >>= filter ( c "type" `elem` kpiCodesWithUserActivity )  
                                      >>= select [(c "p1", "user_id")] 
                                      >>= distinct )  
                              (printf (out_base "daily_uniq_users-%04d-%02d-%02d") y m d )

--sortOut::Feedable a => Selector->[(Exp, File)] -> a -> Either String SortOut

kpi_sorter y m d = sortOut2 $  (kpi_log_raw y m d >>= sortOut  by slots ) 
  where
    by = (Name "date")
    -- from the give day we go back 20 days
    pdayEnd = pDayFromGregorian y m d
    range = [(pdayEnd - 20) .. pdayEnd]
    slots = map (\pday -> let str = showPDayAsGregorian pday in 
                     (SA str, PigFile $ "kpi-" ++ str ++ "__" ++ (show pday))) range
                     
{- 
monthly_uniq_users::MonthlyFile
monthly_uniq_users y m = pig_node ( union [daily_uniq_users y m d | d <- days_of_month y m]  
                                      >>= distinct)
                             (printf (out_base "monthly_uniq_users-%02d-%02d") y m  )

edit_logs::Int->Int->Int->Either String Pipe
edit_logs y m d = kpi_log y m d >>= filter (c "type" `eq` 3)

daily_user_prezi_edits::DaylyFile
daily_user_prezi_edits y m d = pig_node ( edit_logs y m d
                                          >>= select [(c "p1", "user_id"), (c "p2", "prezi_id")]  
                                          >>= freq [(Name "user_id"), (Name "prezi_id")] ) 
                            
                              (printf (out_base "daily_user_prezi_edit_freqs-%04d-%02d-%02d") y m d )

acc_user_prezi_edits y m d =                              

daily_user_prezi_edits2 y m d = pig distinct [daily_user_prezi_edits y m d] (printf (out_base "cucc"))

daily_user_save_counts::DaylyFile
daily_user_save_counts y m d = pig (sum_by 2 [0]) 
                               [daily_user_prezi_edit_freqs y m d]
                               (printf (out_base "daily_user_save_counts" ) y m d )
-}
main = do
  doIt $ kpi_sorter 2012 05 02


    
