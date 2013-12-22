{-# LANGUAGE ExistentialQuantification #-}

import CommonStatistics
import qualified Data.IntMap.Lazy
--import Control.Arrow
import TicTacToe


baseDoFunc :: [String] -> Memory -> (Memory, [String])
baseEvFunc :: [String] -> Memory -> (Memory, String)
baseEvFunc (source:othersources) mem = (2:mem, source)
baseDoFunc ((a:b:[c]):(d:e:[f]):(g:h:[i]):[] ) mem = (1:mem, "1":"2":"3":"4":"5":"6":"7":"8":["9"])
--definitely need a random number generator in there^...
--Don't wanna break the pure functionality though...
--Maybe I should inject a random number as a param. Or maybe a few random numbers.

someFunc =
	let
		st = Statistics {victories = Data.IntMap.Lazy.fromList[(0,0),(1,0),(2,0),(3,0)]}
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
			gameFunc = tttFunc
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

main = putStrLn (show $ [1..5])

data Statistics = Statistics{
	victories :: Data.IntMap.Lazy.IntMap Int--victories by sID
} deriving (Show)

data SimState =
	forall state. SimState {
		state :: state,
		gameFunc :: [Agent] -> state -> (StatUpdate, [Agent], state)
	}--show?

update (SimState s f) agents =
	let
		(upd, ags, newstate) = f agents s
	in
		(upd,SimState{state = newstate, gameFunc = f},ags)

data Simulation = Simulation{
	agents :: [Agent],
	stats :: Statistics,
	--simFunc :: [Agent] -> State -> (StatUpdate, State),
	--simState :: State
	sim :: SimState
}

instance Show Simulation where 
	show (Simulation {agents = ag, stats = stat, sim = state})= show(ag, stat) -- = show (ag, stat, state)

statMerge :: StatUpdate -> Statistics -> Statistics
statMerge upd base = Statistics{
	victories = foldl
		(\datamap key -> Data.IntMap.Lazy.update
			(\count -> Just (count + 1))
			key
			datamap
		)
		(victories base)
		(newVictories upd)
	}

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

--step s = Simulation( foldToStats[(recordStats a b)| a <-
--s.Agentslist, b <- s.Agentslist, a != b],
--[getEvolved of A | A<- (s.Agentlist), A needs evolution])
--multiple evolutions??
