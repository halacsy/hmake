module Graph where
import Data.Time.LocalTime
import Data.Maybe
import System.Log.Logger
import System.Log.Handler.Syslog
import Schema

--import Language (Schema)
-- import Prelude hiding (sequence)
-- import Control.Monad.Parallel
{-# LANGUAGE NoMonomorphismRestriction #-}


{-
cat napi_log-$y-$m-$d | grep "user" > user-$y-$m-$d
cat user-$y-$m-$d=(day_of_month $y $m) | sort | uniq > monthy-user-$y-$m

-}

-- van-e output? meg van-e minden input
-- le_kell_e_futtatni::String->[String]->IO Bool

myLog = warningM  "execution"


type Execution = Bool -> IO String 



type FileName = String
data File = UnixFile FileName  | PigFile FileName deriving Show

nameOfFile::File->FileName
nameOfFile (PigFile name) = name

data Node = 
	InputFile Schema File |
	FileGenerator Schema [Node] File Execution |
    Sorter Schema [Node] [File] Execution


instance Show Node where
    show (InputFile schema file) = "input::" ++ (show file) ++ " AS " ++ show schema
    show (FileGenerator schema inputs file _) = "gen:: " ++ show inputs ++ "schema: " ++ show schema ++ show file



-- later node can inherit schema
schemaOfNode::Node->Schema
schemaOfNode (InputFile s _) = s
schemaOfNode (FileGenerator s _ _ _) = s
schemaOfNode (Sorter s _ _ _) = s

-- this is other viewpoint -> a GenNode gets input, you name the output and gives Node
-- which can connected
--type GenNode = [Node]->File->Node

output::Node->Maybe File
output (InputFile _ f) = Just f
output (FileGenerator _ _ f _)  = Just f


getOutputFiles::[Node]->[File]
getOutputFiles nodes = map fromJust $ filter isJust $ map output nodes


have_to_generate::Node->[Node]->IO Bool
--have_to_generate "user-2012-5-1" _ = return Fa
have_to_generate _ _ = return True


-- selects subgraph which should be executed
-- return either InputFile if no execution needed
-- or FileGenerator otherwise
reduce::Node->IO Node
reduce i@(InputFile _ _) = return i
-- TODO: i donno what to do here
reduce s@(Sorter _ deps output execution) = return s

reduce node@(FileGenerator schema deps output cmd) = 
		do
			-- this must be executed iif output is too old
            too_old <- have_to_generate node deps
            --  any of it's child must be genereted
            childs <-  sequence $ map reduce deps
            let child_to_gen = filter generated childs
            if (null child_to_gen) && (not too_old) then
                    return (InputFile schema output)
                 else 
                    return (FileGenerator schema childs output cmd)
                 where
                 -- maybe there is a shorter version of this
                 generated (InputFile _ _) = False
                 generated _ = True 


execute2::Bool->Node->IO [String]
execute2 _ (InputFile _ _)  = return []
execute2 run (FileGenerator _ deps _ cmd)   = execute' run deps cmd
execute2 run (Sorter _ deps _ cmd)  = execute' run deps cmd
execute' run deps cmd =
    do
        c <- sequence $ map (execute2 run) deps 
        cmdS <- cmd False
        myLog ("starting cmd" ++ cmdS)
        my <- cmd run
        myLog ("end of cmd" ++ my)

        return ( (flatten c) ++ [ my ])
        where
            flatten :: [[a]] -> [a]
            flatten l = foldl (++) [] l
           
execution::Node->IO [Execution]
execution (InputFile _ _) = return []
execution (FileGenerator _ deps o cmd)  = do
        c <- sequence $ map execution deps
        return ( (concat c) ++ [ cmd ])
      


execute::   Bool->Execution -> IO String
execute run cmd = do
    r <- cmd run
    return r

