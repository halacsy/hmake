import Parser
import Logic
import Control.Monad.State(modify, State, execState)
kpi_home = "/var/log/scribe/kpi/"
kpi_file = kpi_home ++ "kpi-$year-$month-$day_00000.gz"
data_home = "/home/hp/data/"
get_user_stream = "awk '($5==0 || $5==1 || $5==2 || $5==3 || $5==4 || $5==5 || $5==6 || $5==10 || $5==11 || $5==13 || $5==14 || $5==17 || $5==19 || $5==20 || $5==21 || $5==30) {print $6}' | sort | uniq -c "
daily_active_users = fp "daily_uniq_users-$year-$month-$day"
monthly_active_users = fp "monthly_uniq_users-$year-$month"

-- f = substitute daily_active_users [("year", "2009"), ("month", "12"), ("day", "02")]


days_of_month::String->[String]
days_of_month m =  map show [1..2]

funnel file oparam function iparam cmd =
	\params ->
		((map (\v -> substitute file (params##(oparam, v)))) (function (params#iparam)) , cmd)

gen4 = gen3 daily_active_users "day" days_of_month "month" "cat"

--(daily_active_users, "day" , days_of_month % "month"
gen2  params =    ((daily_active_users  / "day" $  (days_of_month % "month")), "cat")
	where 
		(%) f p = f (params#p)
		(/) f p = (map (\v -> substitute f (params##(p, v))))
		(<) cmd files params = \p -> (files, cmd)

r= (Rule  monthly_active_users gen4)
	
target = (fp "monthly_uniq_users-$year=2012-$month=01")
main = do
	--print (create_string_template "c")
	--print (create_string_template "c$date")
	--print (create_string_template "c$date=1")
	print r
	print target
	print (name monthly_active_users)
	print (name target)
	print  (anchor_rule target r)