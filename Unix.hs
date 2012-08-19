module Unix (
    ux_cmd,
    ux_pipe,
    cp,
    grep,
    uniq,
    ux_rule
    )

where
import Graph
--import Parser

import System.Process
import Text.Printf
import Data.List


--type Dependencies = [Node]
--type Executable = File->IO String

join delim l = concat (intersperse delim l)

type CmdGen = [Node] -> Cmd

--ux_command::String->[String]->Bool->CmdGen
--ux_command cmd params run =  do

type PipeCmd = [Node]->File->Cmd

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
    

_cp::PipeCmd
-- furdeni akarok, most ez tul van altalanositva, ezert ez lista bemenetu, pedig a copy csak sima fajl kene
-- volt egy valtozat, amikor sima fajl is lehetett bemenet, de aztan azt hittem, h mindennel lehet lista a bemenet
-- mondjuk a copy is lehet concat single fajl
_cp i o = ux_cmd ("cp "  ++ (output $ head i ) ++ " " ++ o)

_grep::String->PipeCmd
_grep pat  = ux_pipe "grep" [pat] 

_uniq::PipeCmd
_uniq = ux_pipe "sort | uniq" [] 


ux_rule::[Node] -> PipeCmd -> File -> Node
ux_rule i cmd o = FileGenerator i o (cmd i o)


grep::String->GenNode
grep  p i o = ux_rule i (_grep p) o

cp::GenNode
cp i o = ux_rule i _cp o

uniq::GenNode
uniq i o = ux_rule i _uniq o
