{- NoMonomorphismRestriction -}
import Language
import PigCmd hiding (join)
import Text.Printf
import Util
import Graph 
import Cli
import Schema
import Prelude hiding (elem, filter, sum)
import Print

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

-- date,time,hostname,logtype,loglevel,client_date,client_time,ip,prezi_oid,user_id,p1,p2,p3);
client_log_schema = [(Just "date", S),
              (Just "time", S), 
              (Just "hostname", S), 
              (Just "logtype", S),
              (Just "loglevel", S),
              (Just "client_date", S),
              (Just "client_time", S),
              (Just "ip", S),
              (Just "oid", S),
              (Just "user_id", I),
              (Just "p1", S),
              (Just "p2", S),
              (Just "p3", S)]

scribeFile::String->PDay->File
scribeFile "client" _ = (PigFile "client.log")
scribeFile category pday = (PigFile ("/scribe/" ++ category ++ "/" ++ category ++ "-" ++ (showPDayAsGregorian pday) ++ "_00*"))

--	kpi_log y m d = Right $ InputFile kpi_schema (PigFile  $ printf "/scribe/kpi/kpi-%04d-%02d-%02d_00000" y m d)
kpi_log_raw::PDay->Either String Node
kpi_log_raw pday = Right $ InputFile kpi_schema (PigFile  $"/scribe/kpi/kpi-" ++ (showPDayAsGregorian pday) ++ "_00*")

client_log::PDay->Either String Node
client_log pday = Right $ InputFile client_log_schema $ scribeFile "client" pday 



daily_detected_languages pday = --storedAsHdfs
                                   --(
                                     client_log pday >>= 
                                      filter (c "p2" `matches` "language_detected.*") >>= 
                                      select [(Exp (substring "p2" 24 29) (Just "language"))] >>= 
                                      freq ["language"]
                                    --) (base ++ "/daily_detected_languages-" ++ show pday )



kpi_code = 4
kpi_user = 5
kpi_prezi = 6

base = "/user/hp/"
--base = "/Users/hp/"
out_base name = base ++ name 

-- log files come in every day. Some log messages are late -> arrive X days later they are about
-- every day can come a fact about any day between today and 20 before

kpi_log_sorted 1241 = Right $ InputFile kpi_schema (PigFile  "vacak1.log")
kpi_log_sorted 1242 = Right $ InputFile kpi_schema (PigFile  "vacak2.log")

kpi_log_sorted pday = storedAsHdfs (union [kpi_log_raw d | d <- [pday .. pday + 10] ] 
                                    >>= filter ( c "date" `eq`  SA (showPDayAsGregorian pday) ))
                                (base ++ "/kpi-sorted-" ++ (show pday)) -- >>= optionalInput


daily_uniq_users  pday = storedAsHdfs (  kpi_log_sorted pday
                                      >>= filter ( c "type" `elem` kpiCodesWithUserActivity )  
                                      >>= cut [("p1", "user_id")] 
                                      >>= distinct )  
                              (base ++ "daily_uniq_users-" ++ (show pday))

facebook_active_users pday =  storedAsHdfs ( union [daily_uniq_users d | d <- [pday - 30 .. pday]]  
                                      >>= distinct)
                              (base ++ "facebook_active_users-" ++ (show pday) )

edit_logs::PDay->Either String Node
edit_logs pday = kpi_log_sorted pday >>= filter (c "type" `eq` kpiSave)


vacak pday =storedAsHdfs ( union [daily_detected_languages day | day <- [pday - 10 .. pday] ]
            >>= group ["language"] (sum "count")
            ) (base ++ "/vacak" ++ show pday)

daily_user_prezi_edits day = storedAsHdfs ( edit_logs day
                                        >>= cut [("p1", "user_id"), ("p2", "prezi_id")] 
                                        >>= freq ["user_id", "prezi_id"]   
                                      )
                              (base ++ "daily_user_prezi_edits-" ++ show day)

acc_user_prezi_edits 1235 = daily_user_prezi_edits 1235

acc_user_prezi_edits day = storedAsHdfs (union [ acc_user_prezi_edits (day-1),
                                                 daily_user_prezi_edits  day
                                               ]
                                     >>= group ["user_id", "prezi_id"] (sum "count") )

                              (base ++ "acc_user_prezi_edits-" ++ show day)

-- prezi+edit, osszes edit
{-}

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
v (Right n) = pigScriptWithStore n "vacak"


main = do
 -- print $ kpi_log_sorted 12
 let range = [ pDayFromGregorian 2012 08 1 .. pDayFromGregorian 2012 08 29   ]
 -- doIt $ doAllOf  [facebook_active_users d | d<- range]
 let cucc = (vacak 1236)
 doIt $ cucc
 --putStr $ v cucc 

    
