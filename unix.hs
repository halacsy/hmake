module Unix (
    ux_cmd,
    ux_pipe,
    cp,
    grep,
    uniq,
    ux_rule
    )

where
import Logic
--import Parser

import System.Process
import Text.Printf
import Data.List


--type Dependencies = [DepGraph]
--type Executable = File->IO String

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

(||)::Cmd->Cmd->Cmd
(||) cmd1 cmd2 = \run -> do
                    res1 <- cmd1 run
                    res2 <- cmd2 run
                    return (res1 ++ "\n" ++ res2)

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


ux_rule::[DepGraph] -> PipeCmd -> File -> DepGraph
ux_rule i cmd o = GeneratedFile i o (cmd i o)
