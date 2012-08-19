module PigCmd where

import Language 
import Graph 

import System.IO
import System.Process
import Prelude hiding (filter)
import Data.List (intersperse)
import Data.Bits
import Control.Monad.Instances
import Print (pigScriptWithStore)
join delim l = concat (intersperse delim l)
{-
{- writeFile :: FilePath -> String -> IO ()
writeFile p = withFile p WriteMode . flip hPutStr
-}
hash :: String -> Int
hash  = abs . foldl' (\h c -> 33*h `xor` fromEnum c) 5381

pig2File::String->String
pig2File x =  "tmp" ++ (show . hash) x  ++ ".pig"
  
dumpPigToTemp::Pipe->IO (String, String)
dumpPigToTemp x = do
    _ <- writeFile fp content 
    return (fp, content)
       where
       content = pretty_print x
       fp = pig2File content

executePig::Pipe->IO (String, String, String)
executePig x = do
    (fn, content) <- dumpPigToTemp x
    putStrLn $ "executing\n" ++ content
    (exit, out, err) <- readProcessWithExitCode "pig" ["-f" , fn] ""
    return (fn, content, out )
    
pig_cmd::Pipe->String->Cmd
pig_cmd expr outFile execute =
    if execute then do
        -- we delete the file/directory as in pig/hadoop you can't overwrite
        (exit, out, err)  <- readProcessWithExitCode "hadoop" ["fs", "-rmr", outFile] ""
        print  $ "delete out" ++ outFile
        (fn, content, output) <- executePig expr
        return (fn ++ ":\n" ++ content ++ "\n-------------\n" ++ output)
    else
        return $ pretty_print expr

-}

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
pig::Transformer->[Node]->FileName->Either String Node
pig  _ [] _  = fail "empty list of input files"
pig trans inputNodes o =
    do{- 
        -- inputs must have the same schema
        if not $ all ((==) inputSchema) $ map schemaOfNode inputNodes then
            fail "not the same schema at all input file"
        else
         -}   -- if there is an error in the script -> this fails
            pipe <- (load globbedInput inputSchema >>= trans)
            let script = pigScriptWithStore pipe
            let outputSchema = schemaOfPipe pipe
            let execution = undefined 
            return (FileGenerator outputSchema inputNodes (PigFile o) execution)
    where
        inputFiles = getOutputFiles inputNodes
        inputSchema = head $ map schemaOfNode inputNodes
        globbedInput = toGlob $ map nameOfFile $ getOutputFiles inputNodes
            
input = (InputFile ([(Just "user_id", I), (Just "linda", S), (Just "freq", I)] ) (PigFile "hello"))
pig_command::Transformer
pig_command = generate [(Selector (Pos 1))]

main = do
    print $ pig (pig_command) [input] "hallo"
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
    