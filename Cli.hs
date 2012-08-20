module Cli where

import Text.Printf
import System( getArgs )
import System.Console.GetOpt
import qualified Data.List
import System.Log.Logger
import Util
import Graph

runWithFlags f target = do
    g <- reduce $ target
    g2 <- execute2 make g
    -- mapM print g2
    print "done"
    where
        make =  Make `Data.List.elem` f

doIt (Left error) =
    fail error

doIt (Right target) = do
  updateGlobalLogger "execute" (setLevel DEBUG)
  warningM "MyApp.Component2" "Something Bad is about to happen."
  args <- getArgs
  case getOpt RequireOrder options args of
    (flags, [],      [])     -> runWithFlags flags target
    (_,     nonOpts, [])     -> error $ "unrecognized arguments: " ++ unwords nonOpts
    (_,     _,       msgs)   -> error $ concat msgs ++ usageInfo header options

data Flag = Version | Make deriving (Eq)

options :: [OptDescr Flag]
options = [ Option ['V'] ["version"] (NoArg Version) "show version number" ,
            Option ['M'] ["make"] (NoArg Make) "actually run commands   "]

header = "Usage: main [OPTION...]"
