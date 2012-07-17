import Parser
import Logic
import Control.Monad.State(modify, State, execState)

-- f = substitute daily_active_users [("year", "2009"), ("month", "12"), ("day", "02")]



funnel file oparam function iparam cmd =
	\params ->
		((map (\v -> substitute file (params##(oparam, v)))) (function (params#iparam)) , cmd)

-- cat input[input_param=generate(output_param)] | cmd > output(output_param)
funnel_rule what from_what output_param generate input_param cmd =
	(Rule what ( funnel  from_what output_param generate input_param cmd))

-- simple cat input | cmd > output
tranform_rule output input cmd =
	(Rule output (\params -> ([substitute input params] , cmd)))


--kpi_home = "/var/log/scribe/kpi/"
kpi_home = "/Users/hp/Documents/Pig/log/"
active_home = "/Users/hp/Documents/Pig/"

kpi f = fp $ kpi_home ++ f
active f = fp $ active_home ++ f


days_of_month::String->String->[String]
days_of_month y m =  map show [1..1]


type Comm = [File]->String
cat::Comm
cat inputs = "cat"

get_uniq_users::Comm
get_uniq_users inputs = "get_uniq_users"

type Param = String
year::Param 
year = "year"

month::Param
month = "month"

day::Param
day = "day"

(###)::File->Param->(File, Param)
(###) f p = (f, p)

(===)::Show a=>(File, String)->a->File
(===) (f, p) v = substitute f (params_from_list [(p, (show v))])

data Unit = F File | Fs [File] 

kpi_file yr m d =
	 F (kpi "kpi-$year-$month-$day_00000.gz")###year===yr###month===m###day===d

daily_active_users yr m d =
	get_uniq_users [ kpi_file yr m d ] 

-- like a rule without name
monthly_active_users2 yr m =
	cat [ daily_active_users yr m day |  day <- days_of_month yr m]



--r= funnel_rule monthly_active_users daily_active_users "day" days_of_month "month" "cat"
--r2 = tranform_rule daily_active_users kpi_file "get_uniq_users_stream"
--rules = [r2, r]

target = monthly_active_users2 "2012" "07"


print_rules rules =
	mapM_ print rules

main = do
	--print (create_string_template "c")
	--print (create_string_template "c$date")
	--print (create_string_template "c$date=1")
	print "target"