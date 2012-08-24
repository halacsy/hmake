module PigCmd where

import Language 
import Graph 

import System.IO
import System.Process
import Prelude hiding (filter)
import Data.List (intersperse, foldl')
import Data.Bits
import Control.Monad.Instances
import Print (pigScriptWithStore)
import System.Exit

join delim l = concat (intersperse delim l)

{- writeFile :: FilePath -> String -> IO ()
writeFile p = withFile p WriteMode . flip hPutStr
-}

hash :: String -> Int
hash  = abs . foldl' (\h c -> 33*h `xor` fromEnum c) 5381

pig2File::String->String
pig2File x =  "tmp" ++ (show . hash) x  ++ ".pig"
  
dumpPigToTemp::String->IO String
dumpPigToTemp script = do
    _ <- writeFile fp script 
    return fp
       where
       fp = pig2File script
  

pig_cmd::Pipe->String->Execution
pig_cmd pipe outFile execute =
    
    if execute then do
        -- we delete the file/directory as in pig/hadoop you can't overwrite
        (exit, out, err) <- readProcessWithExitCode "hadoop" ["fs", "-rmr", outFile] ""
        print  $ "delete out" ++ outFile
        executePig script
    else
        return $ script
    where
        script = pigScriptWithStore pipe outFile
 
executePig::String->IO String
executePig script  = do
    fn <- dumpPigToTemp script
    putStrLn $ "executing\n" ++ script
    (exit, out, err) <- readProcessWithExitCode "pig" ["-f" , fn] ""
    case exit of
        ExitSuccess -> return $ "exit:" ++ (show exit) ++ "\nout:\n" ++ out ++ "\nerr:\n" ++ err
        ExitFailure _ -> fail $ (show exit) ++ err
 

-- this can be more haskell like. Creates from
-- /Users/hp/log-1, /Users/hp/log-2 -> /Users/hp/log-{1,2}
toGlob::[String]->String
toGlob names =
        let 
            prefix = take prefix_len (head names)
            remainders = map (drop prefix_len) names
        in 
            if length names > 1 then
                prefix ++ "{" ++ (join "," remainders) ++ "}"
            else
                head names
    where
        prefix_len = if length names > 1 then aux 1 else 0
        aux n = let pat = take n (head names) in
              if all (\s-> pat == (take n s)) names then (aux (n + 1))  else n  - 1

-- TODO: how to handle input error?
pig::Transformer->[Either String Node]->FileName->Either String Node
pig  _ [] _  = fail "empty list of input files"
pig trans inputNodes o =
    do{- TODO
        -- inputs must have the same schema
        if not $ all ((==) inputSchema) $ map schemaOfNode inputNodes then
            fail "not the same schema at all input file"
        else
         -}   -- if there is an error in the script -> this fails
            inputNodes' <- sequence inputNodes
            let globbedInput = toGlob $ map nameOfFile $ getOutputFiles inputNodes'
            let inputSchema = head $ map schemaOfNode inputNodes'
            pipe <- (load globbedInput inputSchema >>= trans)
            let outputSchema = schemaOfPipe pipe
            let execution = pig_cmd pipe o 
            return (FileGenerator outputSchema inputNodes' (PigFile o) execution)
     

pig_node::Either String Pipe->String->Either String Node
pig_node (Left s) _ = Left s
pig_node (Right pipe) o = -- we need to find the dependencies

        let dependencies =  getPipeDependencies pipe in
        Right $ FileGenerator (schemaOfPipe pipe) dependencies (PigFile o) (pig_cmd pipe o)

{-main = do
    print $ pig (pig_command) [input] "hallo"
-}
{-
one::Int
one = 1
chain =  select 12 Eq "show_kpi" ->> cut [9] ->> select 1 LtE "1300000" ->> freq 1 ->> select 2 Eq one ->> cut [1]
x = chain $ load "VACAK1"

y = pig chain [InputFile "hello"] "hallo"

e::Node->IO String
e (FileGenerator _ _ cmd) = cmd True

main = do 
    res <- e y
    print res
    -}
    