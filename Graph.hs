module Graph where
import Data.Time.LocalTime
import System.Log.Logger
import System.Log.Handler.Syslog
import Prelude hiding (sequence)
import Control.Monad.Parallel
{-# LANGUAGE NoMonomorphismRestriction #-}

{-
cat napi_log-$y-$m-$d | grep "user" > user-$y-$m-$d
cat user-$y-$m-$d=(day_of_month $y $m) | sort | uniq > monthy-user-$y-$m

-}

-- van-e output? meg van-e minden input
-- le_kell_e_futtatni::String->[String]->IO Bool

myLog = warningM  "execution"


type Cmd = Bool -> IO String 
type File = String



data DepGraph = 
	InputFile File |
	GeneratedFile [DepGraph] File Cmd 

-- this is other viewpoint -> a GenNode gets input, you name the output and gives DepGraph
-- which can connected
type GenNode = [DepGraph]->File->DepGraph

output::DepGraph->File
output (InputFile f) = f
output (GeneratedFile _ f _)  = f



have_to_generate::File->[DepGraph]->IO Bool
--have_to_generate "user-2012-5-1" _ = return Fa
have_to_generate _ _ = return True


-- selects subgraph which should be executed
-- return either InputFile if no execution needed
-- or GeneratedFile otherwise
reduce::DepGraph->IO DepGraph
reduce (InputFile f) = return $ InputFile f
reduce (GeneratedFile deps output cmd) = 
		do
			-- this must be executed iif output is too old
            too_old <- have_to_generate output deps
            --  any of it's child must be genereted
            childs <-  sequence $ map reduce deps
            let child_to_gen = filter generated childs
            if (null child_to_gen) && (not too_old) then
                    return (InputFile output)
                 else 
                    return (GeneratedFile childs output cmd)
                 where
                 -- maybe there is a shorter version of this
                 generated (InputFile _) = False
                 generated _ = True 


execute2::Bool->DepGraph->IO [String]
execute2 _ (InputFile _)  = return []
execute2 run (GeneratedFile deps o cmd)   = do
        c <- sequence $ map (execute2 run) deps 
        cmdS <- cmd False
        myLog ("starting cmd" ++ cmdS)
        my <- cmd run
        myLog ("end of cmd" ++ cmdS)

        return ( (flatten c) ++ [ my ])
        where
            flatten :: [[a]] -> [a]
            flatten l = foldl (++) [] l
           
execution::DepGraph->IO [Cmd]
execution (InputFile _) = return []
execution (GeneratedFile deps o cmd)  = do
        c <- sequence $ map execution deps
        return ( (flatten c) ++ [ cmd ])
        where
            flatten :: [[a]] -> [a]
            flatten l = foldl (++) [] l



execute::   Bool->Cmd -> IO String
execute run cmd = do
    r <- cmd run
    putStrLn r
    return r

