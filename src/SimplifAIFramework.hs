{-# LANGUAGE ExistentialQuantification #-}

import CommonStatistics
import qualified Data.IntMap.Lazy
import TicTacToe
import Control.Monad
import Data.Ord
import Data.List

--------------------------------------------------------------------------------
--------------------------------Test Data Below---------------------------------
--------------------------------------------------------------------------------

baseEvFunc :: [String] -> Memory -> (Memory, String)
baseDoFunc :: [String] -> Memory -> (Memory, [String])
baseEvFunc (source:othersources) mem = (2:mem, source)
baseDoFunc ((a:b:[c]):(d:e:[f]):(g:h:[i]):[] ) mem = (1:mem, "1":"2":"3":"4":"5":"6":"7":"8":["9"])
--definitely need a random number generator in there^...

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
--statMerge upd base = Statistics{
--	victories = foldl
--		(\datamap key -> Data.IntMap.Lazy.update
--			(\count -> Just (count + 1))
--			key
--			datamap
--		)
--		(victories base)
--		(newVictories upd)
--	}

data SimState =
	forall state. SimState {
		--agnostic of the actual type of the game's state.
		state :: state,

		--function that steps the simulation
		stepFunc :: [Agent] -> state -> (StatUpdate, [Agent], state)
	}

--updates a SimState/Agents set and produces a StatUpdate in the process
update :: SimState -> [Agent] -> (StatUpdate, SimState, [Agent])
update (SimState s f) agents =
	let
		(upd, ags, newstate) = f agents s
	in
		(upd,SimState{state = newstate, stepFunc = f},ags)

data Simulation = Simulation{
	agents :: [Agent],
	stats :: [Statistics],
	sim :: SimState
}

instance Show Simulation where 
	show (Simulation {agents = ag, stats = stat, sim = state})= show(ag, stat) -- = show (ag, stat, state)

run :: Simulation -> Simulation
run s =
	let
		(statupdate, state, newAgents) = update (sim s) (agents s) 
	in
		Simulation{
			agents = newAgents,
			stats = statMerge statupdate $ stats s,
			sim = state
		}

filterAgents :: [Agent] -> Statistics -> Int -> [Agent]
filterAgents agents stats num = filter (elem flip (take num sortBy (comparing victories) stats)) agents 

scanSubFolder :: [String] -> String -> IO ([(Agent, Int)])
scanSubFolder [] path = return []
scanSubFolder (x:xs) path = liftM2 (++) (scanSubFolder xs path) (scanFolder (path ++ "/" ++ x))

scanFolder :: String -> IO ([(Agent, Int)])
--scanFolder path = scanSubFolder [] path
scanFolder path = do
	contents <- readFile $ path ++ "/Statistics"
	let
		sublines = lines contents
		id = read (filter (`elem` ['0'..'9']) (head (filter (\ line -> (take 2 line) == ":i") sublines)))
		vict = read (filter (`elem` ['0'..'9']) (head (filter (\ line -> (take 2 line) == ":v") sublines)))
		selfCont =
			(Agent{
				agentID = id,
				sourcePath = path ++ "/Source.hs",
				doFunc = baseDoFunc,
				evFunc = baseEvFunc,
				personalMemory = []
			},vict)
	subFolderCont <-
		scanSubFolder
			(map
				(\ (':' : 'c' : line) -> filter (`elem` ['a'..'z']++['0'..'9']) line)
				(filter (\ line -> (take 2 line) == ":c") sublines)) path
	return (selfCont : subFolderCont)

load :: String -> SimState -> IO (Simulation)
load path simIn = do
	folderscan <- scanFolder path
	let
		(loadedagents, tuples) = foldl
			(\ (prevagents, prevtuples) (newagent, newVictories) -> (newagent : prevagents, (agentID newagent,newVictories) : prevtuples))
			([],[])
			folderscan
		--victoryMap = Data.IntMap.Lazy.fromList tuples
	return Simulation{
		agents = loadedagents,
		stats = map (\ (a,b) -> Statistics{aID = a, victories = b}) tuples,
		sim = simIn
	}

testload = load 

