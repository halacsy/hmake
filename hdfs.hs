module Hdfs(last_modified, last_modified2, exist) where
import System.Process
import System.Locale
import System.Exit
import Data.Time.Format
import Data.Time.LocalTime
import System.Posix.Files

last_modified2 :: String-> IO (Maybe LocalTime)
last_modified2 f = do
	
	 (exit, out, err) <- readProcessWithExitCode  "hadoop" ["fs", "-stat", f] ""
	 print out
	 print err
	 print f
	 case exit of
	 	ExitSuccess ->	return (Just $ to_date out)
	 	ExitFailure _ -> return Nothing

last_modified3 :: String -> IO (Maybe LocalTime)
last_modified3 f = do
		stat <- getFileStatus f
		return Nothing -- $ Just  ( modificationTime stat )

last_modified ::String -> IO (Maybe LocalTime)
last_modified f = do
				   return Nothing

exist::String-> IO Bool
exist f = do return False

to_date::String->LocalTime
to_date l = readTime defaultTimeLocale "%F %T" (take 19 l)


main = do
	a <- last_modified3 "/Users/hp/Documents/Pig/log/day-8.log"
	print $   a