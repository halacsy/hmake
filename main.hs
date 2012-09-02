{- NoMonomorphismRestriction -}
import Language
import PigCmd hiding (join)
import Text.Printf
import Util
import Graph 
import Cli
import Schema
import Prelude hiding (elem, filter)

kpiCodesWithUserActivity::[Int]
kpiCodesWithUserActivity = [0,1, 2,3,4,5,6,10,11,13, 14, 17,19, 20, 21, 30]

kpiSave::Int
kpiSave = 3

type DaylyFile = PDay->Either String Node
type MonthlyFile = PMonth -> Either String Node

kpi_schema = [(Just "date", S), 
              (Just "time", S), 
              (Just "hostname", S), 
              (Just "logtype", S), 
              (Just "type", I), 
              (Just "p1", I), 
              (Just "p2", S), 
              (Just "p3", S)] 

--	kpi_log y m d = Right $ InputFile kpi_schema (PigFile  $ printf "/scribe/kpi/kpi-%04d-%02d-%02d_00000" y m d)
kpi_log_raw::PDay->Either String Node
kpi_log_raw pday = Right $ InputFile kpi_schema (PigFile  $"/scribe/kpi/kpi-" ++ (showPDayAsGregorian pday) ++ "_00*")



kpi_code = 4
kpi_user = 5
kpi_prezi = 6

base = "/user/hp/"
--base = "/Users/hp/"
out_base name = base ++ name 

-- log files come in every day. Some log messages are late -> arrive X days later they are about
-- every day can come a fact about any day between today and 20 before

kpi_log_sorted 1242 = Right $ InputFile kpi_schema (PigFile  "vacak.log")
kpi_log_sorted pday = pig_node (union [kpi_log_raw d | d <- [pday .. pday + 5] ] 
                                >>= filter ( c "date" `eq`  SA (showPDayAsGregorian pday) ))
                                (base ++ "/kpi-sorted-" ++ (show pday)) >>= optionalInput

daily_uniq_users  pday = pig_node (  kpi_log_sorted pday
                                      >>= filter ( c "type" `elem` kpiCodesWithUserActivity )  
                                      >>= cut [("p1", "user_id")] 
                                      >>= distinct )  
                              (base ++ "daily_uniq_users-" ++ (show pday))

facebook_active_users pday =  pig_node ( union [daily_uniq_users d | d <- [pday - 30 .. pday]]  
                                      >>= distinct)
                              (base ++ "facebook_active_users-" ++ (show pday) )

edit_logs::PDay->Either String Pipe
edit_logs pday = kpi_log_sorted pday >>= filter (c "type" `eq` kpiSave)


daily_user_prezi_edits day = pig_node ( edit_logs day
                                        >>= cut [("p1", "user_id"), ("p2", "prezi_id")] 
                                        >>= freq ["user_id", "prezi_id"]   
                                        >>= groupByCol "user_id")
                              (base ++ "daily_user_prezi_edits-" ++ show day)

acc_user_prezi_edits 1240 = daily_user_prezi_edits 1240

acc_user_prezi_edits day = pig_node (join (daily_user_prezi_edits (day-1))
                                          (daily_user_prezi_edits day) "user_id")
                              (base ++ "acc_user_prezi_edits-" ++ show day)
-- prezi+edit, osszes edit

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

                             

daily_user_prezi_edits2 y m d = pig distinct [daily_user_prezi_edits y m d] (printf (out_base "cucc"))

daily_user_save_counts::DaylyFile
daily_user_save_counts y m d = pig (sum_by 2 [0]) 
                               [daily_user_prezi_edit_freqs y m d]
                               (printf (out_base "daily_user_save_counts" ) y m d )
-}
main = do
 -- print $ kpi_log_sorted 12
 let range = [ pDayFromGregorian 2012 08 1 .. pDayFromGregorian 2012 08 29   ]
-- doIt $ doAllOf  [facebook_active_users d | d<- range]
 doIt $ daily_user_prezi_edits 1242

    
