import Parser
import Logic
import Control.Monad.State(modify, State, execState)

-- f = substitute daily_active_users [("year", "2009"), ("month", "12"), ("day", "02")]



funnel file oparam function iparam cmd =
	\params ->
		((map (\v -> substitute file (params##(oparam, v)))) (function (params#iparam)) , cmd)

funnel_rule what from_what output_param generate input_param cmd =
	(Rule what ( funnel  from_what output_param generate input_param cmd))

tranform_rule what from_what cmd =
	(Rule what (\params -> ([substitute from_what params] , cmd)))

kpi_home = "/var/log/scribe/kpi/"
kpi_file = fp $ kpi_home ++ "kpi-$year-$month-$day_00000.gz"
data_home = "/home/hp/data/"
get_uniq_users_stream = "awk | sort | uniq"
daily_active_users = fp "daily_active_users-$year-$month-$day"
monthly_active_users = fp "monthly_active_users-$year-$month"
bimontly_uniq_users = fp "bimontly_uniq_users-$year-$month"

-- of cource NEVER USE THIS as it only works from Feb to Dec
previous_month::String->String
previous_month m = show $ ((read m)::Int)  - 1

days_of_month::String->[String]
days_of_month m =  map show [1..1]

this_and_previous_month m = [previous_month m , m]


r= funnel_rule monthly_active_users daily_active_users "day" days_of_month "month" "cat"
r2 = tranform_rule daily_active_users kpi_file get_uniq_users_stream
r3= funnel_rule bimontly_uniq_users monthly_active_users "month" this_and_previous_month "month" "cat | sort | uniq"
rules = [r2, r, r3]

target = (fp "bimontly_uniq_users-$year=2012-$month=07")


print_rules rules =
	mapM_ print rules

main = do
	--print (create_string_template "c")
	--print (create_string_template "c$date")
	--print (create_string_template "c$date=1")
	print "hello"
	rules <- make rules target
	print (length rules)
	print_rules  rules