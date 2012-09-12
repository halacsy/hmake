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

data FileDependency = NonDependent File Node | Dependent Dependency [File] Node


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

-- one level back, not all files
triggeringFiles::Node->[File]
triggeringFiles (InputFile _ f) = [f]
triggeringFiles (Transformer (Just f) _ _) = [f]
triggeringFiles (Transformer Nothing _ pipe) = concat $ map triggeringFiles $ getDependencies pipe


data Reason = Fresh | TargetMissing | RequiredSourceMissing File | TargetIsOlderThan File deriving (Show)
haveToGenerateFromTimes::Maybe EpochTime->Bool->[(Maybe EpochTime, File)]->Reason
haveToGenerateFromTimes Nothing _ _ =  TargetMissing
haveToGenerateFromTimes _ _ [] = Fresh
haveToGenerateFromTimes t allNeeded ((Nothing, file):xs) = if allNeeded then RequiredSourceMissing file
                                                           else haveToGenerateFromTimes t allNeeded xs
haveToGenerateFromTimes (Just t1) allNeeded ((Just t2, file):xs) = if t1 < t2 then TargetIsOlderThan file
                                                                   else haveToGenerateFromTimes (Just t1) allNeeded xs


have_to_generate::File->Dependency->IO Bool
--have_to_generate "user-2012-5-1" _ = return Fa
have_to_generate f dependency = do
        targetTime <- modTime f
        
        let (nodes, allNeeded) = case dependency of 
                                    All nodes -> (nodes, True)
                                    Any nodes -> (nodes, False)
        let sourceFiles = concatMap triggeringFiles nodes
        sourcesTime <- mapM modTime sourceFiles
        let sources = zip sourcesTime sourceFiles

        
        case haveToGenerateFromTimes targetTime allNeeded sources of
            Fresh -> do 
                        myLog $ "file is fresh" ++ (show f)
                        return False
            x     -> do
                        myLog $ "file needs to be generated " ++ show f ++ show x
                        return True
    

type ExecutionStep = (File, Node)
data ExecutionPlan = Nil | ExecutionPlan [ExecutionPlan] (Maybe ExecutionStep) deriving (Show, Eq)
{-}
emptyPlan::ExecutionPlan->Bool
emptyPlan Nil = True
emptyPlan (ExecutionPlan [] Nothing) = True
emptyPlan (ExecutionPlan [] (Just _)) = False
emptyPlan (ExecutionPlan children _) = and $ map emptyPlan children
-}
reduce::Node->IO ExecutionPlan
reduce i@(InputFile _ _) = return Nil
reduce t@(TaskGroup nodes) =  do
                                children <- sequence $ map reduce nodes
                                return $ ExecutionPlan children Nothing

reduce node@(Transformer Nothing _ pipeCmd ) = do
        children <-  mapM reduce (getDependencies pipeCmd)
        let children' = filter ((/=) Nil) children
        if not $ null children' then
            return $ ExecutionPlan children' Nothing
        else
            return Nil

reduce node@(Transformer (Just output) schema pipeCmd) = 
		do
            let deps = getDependencies pipeCmd
            -- this must be executed iif output is too old
            too_old <- have_to_generate output (All deps)
              

            --  any of it's child must be genereted
            
            children <-  mapM reduce deps
            let children_to_gen = filter ((/=) Nil) children
          
          {-  print node
            print "too old"
            print too_old
            print "deps"
            print deps
            print "children"
            print children
            print "children to gen"
            print children_to_gen -}
            if (null children_to_gen) && (not too_old) then do
                 --   print "returning Nill\n" 
                    return Nil
                 else 
                    -- TODO: theoritically output can't be Nothing here but should
                    -- be verified by the type system, no?
                    return $ ExecutionPlan children_to_gen $ Just (output, node)
                 
execute2::Bool->ExecutionPlan->IO [String]
execute2 _ Nil  = return []

execute2 run (ExecutionPlan preds execution) =
    do
        c <- sequence $ map (execute2 run) preds 
        my <- case execution of
                Nothing -> return []
                Just ((PigFile file), pipe) -> let cmd = (pig_cmd pipe) in cmd run

        return ( (flatten c) ++ [ my ])
        where
            flatten :: [[a]] -> [a]
            flatten l = foldl (++) [] l
           

