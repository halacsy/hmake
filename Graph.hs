module Graph where
import Data.Time.LocalTime
import Data.Maybe
import System.Log.Logger
import System.Log.Handler.Syslog
import Schema
import Language hiding (filter)
import PigCmd (pig_cmd)
import System.Posix.Types(EpochTime)
import System.Posix.Files(modificationTime, getFileStatus)
import System.IO.Error 
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




-- this is other viewpoint -> a GenNode gets input, you name the output and gives Node
-- which can connected
--type GenNode = [Node]->File->Node
-- TODO why is this Maybe


modTime::File->IO (Maybe EpochTime)
modTime f = do

    stat <- try $ getFileStatus $ getActualFile f
    case stat of
        Left ex -> return Nothing
        Right stat ->  return $ Just $ modificationTime stat

haveToGenerateFromTimes::Maybe EpochTime->Bool->[Maybe EpochTime]->Bool
haveToGenerateFromTimes Nothing _ _ =  True
haveToGenerateFromTimes _ _ [] = False
haveToGenerateFromTimes t allNeeded (Nothing:xs) = allNeeded || haveToGenerateFromTimes t allNeeded xs
haveToGenerateFromTimes (Just t1) allNeeded (Just t2:xs) = if t1 < t2 then True else haveToGenerateFromTimes (Just t1) allNeeded xs

have_to_generate::File->Dependency->IO Bool
--have_to_generate "user-2012-5-1" _ = return Fa
have_to_generate f dependency = do
        targetTime <- modTime f
        let (nodes, allNeeded) = case dependency of 
                                    All nodes -> (nodes, True)
                                    Any nodes -> (nodes, False)

        sourcesTime <- sequence . map modTime $ getOutputFiles nodes
        return $ haveToGenerateFromTimes targetTime allNeeded sourcesTime 
    


-- selects subgraph which should be executed
-- return either InputFile if no execution needed
-- or FileGenerator otherwise
reduce::Node->IO Node
reduce i@(InputFile _ _) = return i
reduce t@(TaskGroup nodes) =  do
                                children <- sequence $ map reduce nodes
                                return $ TaskGroup children

reduce node@(FileGenerator schema deps output cmd) = 
		do
			-- this must be executed iif output is too old
            too_old <- have_to_generate output deps
            --  any of it's child must be genereted
            children <-  sequence $ map reduce $ source deps
            let children_to_gen = filter generated children
            if (null children_to_gen) && (not too_old) then
                    return (InputFile schema output)
                 else 
                    return (FileGenerator schema (All children) output cmd)
                 where
                 -- maybe there is a shorter version of this
                 generated (InputFile _ _) = False
                 generated _ = True 


execute2::Bool->Node->IO [String]
execute2 _ (InputFile _ _)  = return []
execute2 run (TaskGroup nodes) = do 
                                    c <- sequence $ map (execute2 run) nodes
                                    return $ concat c
execute2 run (FileGenerator _ deps (PigFile o) pipe)   = execute' run deps o pipe
execute' run deps o pipe =
    do
        c <- sequence $ map (execute2 run) (source deps) 
        let cmd = (pig_cmd pipe o)
        my <- cmd run
      --  myLog ("end of cmd" ++ my)

        return ( (flatten c) ++ [ my ])
        where
            flatten :: [[a]] -> [a]
            flatten l = foldl (++) [] l
           
execute::   Bool->Execution -> IO String
execute run cmd = do
    r <- cmd run
    return r

