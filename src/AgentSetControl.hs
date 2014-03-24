module AgentSetControl where

import Control.Monad
import Data.Ord
import Data.List
import System.IO
import CommonStatistics
--import RuntimeCompiler

--------------------------------------------------------------------------------
--------------------------------Test Data Below---------------------------------
--------------------------------------------------------------------------------

--TODO: delete this part, comment in the rtCompiler parts once that's done.

baseEvFunc :: [String] -> Memory -> (Memory, String)
baseDoFunc :: [String] -> Memory -> (Memory, [String])
baseEvFunc (source:othersources) mem = (2:mem, source)
baseDoFunc ((a:b:[c]):(d:e:[f]):(g:h:[i]):[] ) mem = (1:mem, "1":"2":"3":"4":"5":"6":"7":"8":["9"])

--------------------------------------------------------------------------------
--------------------------------Test Data Above---------------------------------
--------------------------------------------------------------------------------

storeAndMakeAgent :: String -> String -> String -> Int -> IO(Agent)
--path and subset terminate with /
storeAndMakeAgent sourcecode path subset parentID = do
	id <- getNextAgentID path
	let sourcepath = path ++ subset ++ (show id) ++ "src.hs"
	--create file if need be?!
	srchandle <- openFile sourcepath WriteMode
	hPutStr srchandle sourcecode
	hClose srchandle
	stathandle <- openFile (path ++ subset ++ (show id) ++ "Statistics") WriteMode
	hPutStr stathandle $ getNewStats id parentID
	hClose stathandle
	--(doF, evF) <- compile sourcepath --TODO: I just want to live.
	let (doF, evF) = (baseDoFunc,baseEvFunc)--TODO: KILL ME!
	return Agent{
		agentID = id,
		sourcePath = sourcepath,
		doFunc = doF,
		evFunc = evF,
		personalMemory = []
	}

getNewStats :: Int -> Int -> String
getNewStats id parid = unlines [(":i " ++ (show id)), ":v 0", (":p " ++ (show parid))]

getNextAgentID :: String -> IO (Int)
getNextAgentID path = do
	filehandle <- openFile (path ++ "/globalstats") ReadWriteMode
	file <- hGetContents filehandle
	let id = read $ filter (`elem` ['0'..'9']) $ head (filter 
			(\line -> (take 6 line) == ":idmax")
			(lines file)
		)
	print id
	let newlines = map
		(\line ->
			if (take 6 line) == ":idmax"
			then ":idmax " ++ show (id+1)
			else line
		)
		(lines file)
	hClose filehandle
	filehandle2 <- openFile (path ++ "/globalstats") WriteMode
	writable <- hIsWritable filehandle2
	hPutStr filehandle2 $ unlines newlines
	hClose filehandle2
	return (id+1)
