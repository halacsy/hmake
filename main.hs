{- NoMonomorphismRestriction -}
import Logic
--import Parser

import System.Process
import Text.Printf
import Data.List


type Dependencies = [DepGraph]
type Executable = File->IO String

join delim l = concat (intersperse delim l)

type CmdGen = [DepGraph] -> Cmd

--ux_command::String->[String]->Bool->CmdGen
--ux_command cmd params run =  do

type PipeCmd = [DepGraph]->String->Cmd

ux_cmd::String->Cmd
ux_cmd cmdS = \run -> do -- TODO: needs to be simplified. I just don't know haskell enough
                        _ <- if run then do
                               _ <- createProcess $ shell cmdS 
                               return True
                             else return False
                        return cmdS


ux_pipe::String->[String]->PipeCmd
ux_pipe cmd params input o = ux_cmd cmdS
            where 
                cmdS =  "cat " ++ ( join " " (map output input) ) ++ " | " ++ cmd  ++ " " ++ ( join " " params) ++ " > " ++ o
    

cp::[DepGraph]->File->Cmd
-- furdeni akarok, most ez tul van altalanositva, ezert ez lista bemenetu, pedig a copy csak sima fajl kene
-- volt egy valtozat, amikor sima fajl is lehetett bemenet, de aztan azt hittem, h mindennel lehet lista a bemenet
-- mondjuk a copy is lehet concat single fajl
cp i o = ux_cmd ("cp "  ++ (output $ head i ) ++ " " ++ o)

grep::String->PipeCmd
grep pat  = ux_pipe "grep" [pat] 

uniq::PipeCmd
uniq = ux_pipe "sort | uniq" [] 


rule::[DepGraph] -> PipeCmd -> File -> DepGraph
rule i =
    \cmd ->
            \o ->
                GeneratedFile i o (cmd i o)

napi_log::Int->Int->Int->DepGraph
napi_log y m d = -- should be guards
    if (y, m, d) == (2012,4, 19) then
        InputFile $ printf "~/Documents/Pig/log/kpi-%04d-%02d-%02d_00000" y m d 
    else
        rule [napi_log 2012 4 19] cp (printf "~/Documents/Pig/log/kpi_gen-%04d-%02d-%02d_00000" y m d )

user y m d = rule [napi_log y m d] (grep "3") (printf "~/Documents/Pig/log/user-%04d-%02d-%02d_00000" y m d )
monthly_user y m  = rule [user y m d | d <- day_of_month y m] uniq (printf "~/Documents/Pig/log/monthly-%04d-%02d" y m  )

day_of_month y m = [1..31]



main = do 
    g <- select $ monthly_user 2012 04 
    g2 <- execution g
    mapM_  ( (execute True)) g2
    
