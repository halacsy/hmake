import Pig.Dsl
import Pig.Print
import Pig.Language

import System.IO
import System.Process
import Data.List
import Data.Bits
{- writeFile :: FilePath -> String -> IO ()
writeFile p = withFile p WriteMode . flip hPutStr
-}
hash :: String -> Int
hash  = abs . foldl' (\h c -> 33*h `xor` fromEnum c) 5381

pig2File::String->String
pig2File x =  "tmp" ++ (show . hash) x  ++ ".pig"
  
writePig::PigExpr->IO String
writePig x = do
	_ <- writeFile fp content 
	return fp
	   where
	   content = pretty_print x
	   fp = pig2File content

executePig::PigExpr->IO String
executePig x = do
    fn <- writePig x
    _ <- createProcess $ shell ("pig -f " ++ fn)
    return "ok"
    
one::Int
one = 1
chain =  select 12 Eq "show_kpi" ->> cut [9] ->> select 1 LtE "1300000" ->> freq 1 ->> select 2 Eq one ->> cut [1]
x = chain $ load "VACAK1"

main = do 
    res <- executePig x 
    print res
    