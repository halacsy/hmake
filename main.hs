{- NoMonomorphismRestriction -}
import Graph 
import Unix
import PigCmd 
import Pig.Dsl
import Pig.Language
import Prelude hiding (elem)
import Text.Printf
import System( getArgs )
import System.Console.GetOpt
import qualified Data.List
import System.Log.Logger


kpiCodesWithUserActivity::[Int]
kpiCodesWithUserActivity = [0,1, 2,3,4,5,6,10,11,13, 14, 17,19, 20, 21, 30]

kpi_log y m d = InputFile $ printf "/scribe/kpi/kpi-%04d-%02d-%02d_00000" y m d

daily_uniq_users ::Int->Int->Int->DepGraph
daily_uniq_users  y m d = pig ( elem 5 kpiCodesWithUserActivity ->> distinct [6] )  [kpi_log y m d] (printf "/user/hp/daily_uniq_users-%04d-%02d-%02d" y m d )

monthly_uniq_users::Int->Int->DepGraph
monthly_uniq_users y m = pig (distinct [0]) [daily_uniq_users y m d | d <- day_of_month y m] (printf "/user/hp/monthly_uniq_users-%04d-%02d" y m  )
day_of_month y m = [1..5]

target = monthly_uniq_users 2012 04



runWithFlags f = do
    g <- reduce $ target
    g2 <- execute2 dontmake g
    mapM print g2
    where
        dontmake = not $ DontMake `Data.List.elem` f

main = do
  updateGlobalLogger "execute" (setLevel DEBUG)
  warningM "MyApp.Component2" "Something Bad is about to happen."
  args <- getArgs
  case getOpt RequireOrder options args of
    (flags, [],      [])     -> runWithFlags flags
    (_,     nonOpts, [])     -> error $ "unrecognized arguments: " ++ unwords nonOpts
    (_,     _,       msgs)   -> error $ concat msgs ++ usageInfo header options

data Flag = Version | DontMake deriving (Eq)

options :: [OptDescr Flag]
options = [ Option ['V'] ["version"] (NoArg Version) "show version number" ,
            Option ['D'] ["dontmake"] (NoArg DontMake) "don't actually run commands   "]

header = "Usage: main [OPTION...]"

    
