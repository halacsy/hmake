module PigCmd where

import Pig.Dsl
import Pig.Print
import Pig.Language
import Graph

import System.IO
import System.Process
import Data.List
import Data.Bits

join delim l = concat (intersperse delim l)

{- writeFile :: FilePath -> String -> IO ()
writeFile p = withFile p WriteMode . flip hPutStr
-}
hash :: String -> Int
hash  = abs . foldl' (\h c -> 33*h `xor` fromEnum c) 5381

pig2File::String->String
pig2File x =  "tmp" ++ (show . hash) x  ++ ".pig"
  
dumpPigToTemp::PigExpr->IO (String, String)
dumpPigToTemp x = do
    _ <- writeFile fp content 
    return (fp, content)
       where
       content = pretty_print x
       fp = pig2File content

executePig::PigExpr->IO (String, String, String)
executePig x = do
    (fn, content) <- dumpPigToTemp x
    (exit, out, err) <- readProcessWithExitCode "pig" ["-f" , fn] ""
    return (fn, content, out )
    
pig_cmd::PigExpr->String->Cmd
pig_cmd expr outFile execute =
    if execute then do
        -- we delete the file/directory as in pig/hadoop you can't overwrite
        (exit, out, err)  <- readProcessWithExitCode "hadoop" ["fs", "-rmr", outFile] ""
        print  $ "delete out" ++ outFile
        (fn, content, output) <- executePig expr
        return (fn ++ ":\n" ++ content ++ "\n-------------\n" ++ output)
    else
        return $ pretty_print expr

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

pig::PFilter->GenNode
pig exp inputs o = GeneratedFile inputs o cmd
    where 
        cmd = pig_cmd ( exp ->> (store o) $ load i) o
        i = toGlob $ map output inputs
{-
main = do
    print $ toGlob ["/hello"]

one::Int
one = 1
chain =  select 12 Eq "show_kpi" ->> cut [9] ->> select 1 LtE "1300000" ->> freq 1 ->> select 2 Eq one ->> cut [1]
x = chain $ load "VACAK1"

y = pig chain [InputFile "hello"] "hallo"

e::DepGraph->IO String
e (GeneratedFile _ _ cmd) = cmd True

main = do 
    res <- e y
    print res
    -}
    