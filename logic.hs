module Logic where
import Data.Time.LocalTime

{-# LANGUAGE NoMonomorphismRestriction #-}


{-
cat napi_log-$y-$m-$d | grep "user" > user-$y-$m-$d
cat user-$y-$m-$d=(day_of_month $y $m) | sort | uniq > monthy-user-$y-$m

-}

-- van-e output? meg van-e minden input
-- le_kell_e_futtatni::String->[String]->IO Bool

type Cmd = ()-> IO String 
type File = String
isMoreRecent (dep:deps) reference = True

generateCommand deps myOutput cmd
  | deps `isMoreRecent` myOutput = [cmd]
  | otherwise                    = []



data DepGraph = 
	InputFile File |
	GeneratedFile [DepGraph] File Cmd 

output::DepGraph->File
output (InputFile f) = f
output (GeneratedFile _ f _)  = f

have_to_generate::File->[DepGraph]->IO Bool
have_to_generate "user-2012-5-1" _ = return True
have_to_generate _ _ = return False


-- selects subgraph which should be executed
-- return either InputFile if no execution needed
-- or GeneratedFile otherwise
select::DepGraph->IO DepGraph
select (InputFile f) = return $ InputFile f
select (GeneratedFile deps output cmd) = 
		do
			-- this must be executed iif output is too old
            too_old <- have_to_generate output deps
            --  any of it's child must be genereted
            childs <-  sequence $ map select deps
            let child_to_gen = filter generated childs
            if (null child_to_gen) && (not too_old) then
                    return (InputFile output)
                 else 
                    return (GeneratedFile childs output cmd)
                 where
                 -- maybe there is a shorter version of this
                 generated (InputFile _) = False
                 generated _ = True 




execution::DepGraph->IO [([File], Cmd, File) ]
execution (InputFile _) = return []
execution (GeneratedFile deps o cmd)  = do
        c <- sequence $ map execution deps
        return ( (flatten c) ++ [ (inputs, cmd, o )])
        where
            flatten :: [[a]] -> [a]
            flatten l = foldl (++) [] l
            inputs = map output deps


napi_log y m d = InputFile $ "napi_log-" ++ show y ++ "-" ++ show m ++ "-" ++ show d

user y m d = GeneratedFile deps myOutput cmd
  where
    cmd = grep_command (napi_log y m d)
    deps = [napi_log y m d]
    myOutput = "user-" ++ show y ++ "-" ++ show m ++ "-" ++ show d

grep_command::DepGraph->()->IO String
grep_command (InputFile f) _ = return ("grep " ++ f)

monthly_user y m = GeneratedFile deps myOutput cmd
  where
    cmd = \_ -> return "concat"
    deps = [user y m d | d <- day_of_month y m]
    myOutput = "monthly-user-" ++ show y ++ "-" ++ show m

day_of_month y m = [1..2]

execute:: ([File], Cmd, File) -> IO String
execute (inputs, cmd, o) = do
    r <- cmd ()
    print r
    return r

a::()->IO String
a _ = return "hello"

main = do 
    g <- select $ monthly_user 2012 5
    g2 <- execution g
    e <- a ()
    print e
    mapM_  execute g2
