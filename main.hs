import Parser
import Logic

kpi_home = "/var/log/scribe/kpi/"
kpi_file = kpi_home ++ "kpi-$year-$month-$date_00000.gz"
data_home = "/home/hp/data/"
get_user_stream = "awk '($5==0 || $5==1 || $5==2 || $5==3 || $5==4 || $5==5 || $5==6 || $5==10 || $5==11 || $5==13 || $5==14 || $5==17 || $5==19 || $5==20 || $5==21 || $5==30) {print $6}' | sort | uniq -c "
active_users = fp "daily_uniq_users-$year-$month-$day"

f  = substitute active_users "year" "2009"

rules = build_rules $ do
	add_rule "a$date/$month" "sort" "b/$date/$month"
	add_rule "e$date" "sort" "g$date"
	add_rule "b/$date/$month" "uniq -c" "c$date$month"
	add_bi_rule "a$date" "c$date$month" "paste" "d$date/$month"
main = do
	--print (create_string_template "c")
	--print (create_string_template "c$date")
	--print (create_string_template "c$date=1")
	print  f