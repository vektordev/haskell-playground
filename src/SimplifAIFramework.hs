{-# LANGUAGE ExistentialQuantification #-}

import CommonStatistics
import qualified Data.IntMap.Lazy
import TicTacToe
import Control.Monad
import Data.Ord
import Data.List
import System.IO
import AgentSetControl

--------------------------------------------------------------------------------
--------------------------------Test Data Below---------------------------------
--------------------------------------------------------------------------------
someFunc =
	let
		st = map (\ (a,b) -> Statistics{aID = a, victories = b}) [(0,0),(1,0),(2,0),(3,0)]
--Statistics {victories = Data.IntMap.Lazy.fromList[(0,0),(1,0),(2,0),(3,0)]}
		ag =
			[
				Agent{
					agentID = 1,
					sourcePath = "path",
					doFunc = baseDoFunc,
					evFunc = baseEvFunc,
					personalMemory = []
				},
				Agent{
					agentID = 3,
					sourcePath = "3path",
					doFunc = baseDoFunc,
					evFunc = baseEvFunc,
					personalMemory = []
				}
			]
		state = SimState{
			state = (),
			stepFunc = tttFunc
		}
		sim = Simulation{
			agents = ag,
			stats= st,
			sim= state
		}
	in sim

--testLoad = load ["../data/default/"] $ sim someFunc

test =
	let (upd, state, ag) = update (sim someFunc) (agents someFunc)
	in Simulation{
		agents = ag,
		stats = statMerge upd (stats someFunc),
		sim= state
	}

--------------------------------------------------------------------------------
--------------------------------Test Data Above---------------------------------
--------------------------------------------------------------------------------

--test I guess
main = putStrLn (show $ [1..5])

data Statistics = Statistics{
	aID :: Int,
	victories :: Int--count of victories by agentID
} deriving (Show)

--merges a StatUpdate into Statistics
statMerge :: StatUpdate -> [Statistics] -> [Statistics]
statMerge upd base = foldl
	(\base2 updAID -> (map
		(\ baseElem -> if aID baseElem == updAID then  Statistics { aID = updAID, victories = 1+ victories baseElem} else baseElem)
		base2))
	base
	(newVictories upd)

data SimState =
	forall state. SimState {
		--agnostic of the actual type of the game's state.
		state :: state,

		--function that steps the simulation
		stepFunc :: [Agent] -> state -> (StatUpdate, [Agent], state)
	}

--updates a SimState/Agents set and produces a StatUpdate in the process
update :: SimState -> [Agent] -> (StatUpdate, SimState, [Agent])
update (SimState state func) agents =
	let
		(upd, ags, newstate) = func agents state
	in
		(upd,SimState{state = newstate, stepFunc = func},ags)

data Simulation = Simulation{
	agents :: [Agent],
	stats :: [Statistics],
	sim :: SimState
}

instance Show Simulation where 
	show (Simulation {agents = ag, stats = stat, sim = state})= show(ag, stat)

data Options = Options{
	game :: String,
	gameParams :: [String],
	agentSets :: [String],
	numTurns :: Int,
	path :: String
}

--here be language shenanigans
--to = id
--add n to m = m+n
--fancy function syntax. consider "to" to be part of the function name. ;-)

getSimulation :: String -> SimState
getSimulation "TicTacToe" = SimState{
	state = (),
	stepFunc = tttFunc
}

step :: Simulation -> Int -> IO (Simulation)
step s 0 = return s
step s steps =
	let
		(statupdate, state, newAgents) = update (sim s) (agents s) 
	in (step (Simulation{
		agents = newAgents,
		stats = statMerge statupdate $ stats s,
		sim = state
	}) (steps-1))

run :: Options -> IO ()
run opt = do
	let
		initialstate = Simulation { agents = [], stats = [], sim = getSimulation $ game opt }
	fullSim <- (load [(path opt) ++ "default/"] initialstate) :: IO(Simulation)
	let
		a = step fullSim (numTurns opt)
	return ()

filterAgents :: [Agent] -> [Statistics] -> Int -> [Agent]
--sort Agents, filter out the weaker ones, return a list of the num best Agents
filterAgents agents stats num =
	let
		sorted = sortBy (comparing victories) stats
		sortedandfiltered = take num sorted
	in filter (\agent -> elem (agentID agent) (map aID sortedandfiltered)) agents


--scanSubFolder :: [String] -> String -> IO ([(Agent, Int)])
--scanSubFolder [] path = return []
--scanSubFolder (x:xs) path = liftM2 (++) (scanSubFolder xs path) (scanFolder (path ++ "/" ++ x))

--scanFolder :: String -> IO ([(Agent, Int)])
--scanFolder path = scanSubFolder [] path
--scanFolder path = do
--	contents <- readFile $ path ++ "/Statistics"
--	let
--		sublines = lines contents
--		id = read (filter (`elem` ['0'..'9']) (head (filter (\ line -> (take 2 line) == ":i") sublines)))
--		vict = read (filter (`elem` ['0'..'9']) (head (filter (\ line -> (take 2 line) == ":v") sublines)))
--		selfCont =
--			(Agent{
--				agentID = id,
--				sourcePath = path ++ "/Source.hs",
--				doFunc = baseDoFunc,
--				evFunc = baseEvFunc,
--				personalMemory = []
--			},vict)
--	subFolderCont <-
--		scanSubFolder
--			(map
--				(\ (':' : 'c' : line) -> filter (`elem` ['a'..'z']++['0'..'9']) line)
--				(filter (\ line -> (take 2 line) == ":c") sublines)) path
--	return (selfCont : subFolderCont)

parseAgent :: (String, String) -> (Agent, Int)
parseAgent (file, path) =
	let
		sublines = lines file
		id = read $ filter (`elem` ['0'..'9']) $ head $ filter (\line -> ":i" == (take 2 line) ) sublines
		vict = read $ filter (`elem` ['0'..'9']) $ head $ filter (\line -> ":v" == (take 2 line) ) sublines
	in
		(Agent{
			agentID = id,
			sourcePath = path,
			doFunc = baseDoFunc,
			evFunc = baseEvFunc,
			personalMemory = []
		},vict)

scanFolder :: String -> IO ([(Agent, Int)])
--path should have / as last char
scanFolder path = do
	contentlist <- readFile $ path ++ "AgentSetStats"
	let
		agentIDs = map
			(\ text -> read $ filter (`elem` ['0'..'9']) text)
			(filter
				(\line -> (take 2 line) == ":i")
				(lines contentlist)
			) :: [Int]
		paths = [path ++ (show id) ++ "Statistics"| id <- agentIDs]
	rawTexts <- sequence (map (\path -> readFile path) paths) :: IO [String]
	return $ map  parseAgent $ zip rawTexts [path ++ (show id) ++ "src.hs" | id <- agentIDs]

load :: [String] -> Simulation -> IO (Simulation)
load paths simIn = do
	folderscan <- (liftM (foldl (++) []) $ mapM scanFolder paths)	
	let
		(loadedagents, tuples) = foldl
			(\ (prevagents, prevtuples) (newagent, newVictories) -> (newagent : prevagents, (agentID newagent,newVictories) : prevtuples))
			([],[])
			folderscan
	return Simulation{
		agents = loadedagents ++ agents simIn,
		stats = map (\ (a,b) -> Statistics{aID = a, victories = b}) tuples ++ stats simIn,
		sim = sim simIn
	}

getAgentSets :: String -> IO([String])
getAgentSets path = do
	file <- readFile $ path ++ "/globalstats"
	let subfolders = (map
		(filter
			(`elem` (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']))
		)
		(map (drop 4) $ filter
			(\line -> (take 4 line) == ":set")
			(lines file)
		)) :: [String]
	return subfolders
