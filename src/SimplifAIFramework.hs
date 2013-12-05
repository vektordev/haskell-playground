{-# LANGUAGE ExistentialQuantification #-}

import CommonStatistics
import qualified Data.IntMap.Lazy
import Control.Arrow

data Statistics = Statistics{
	victories :: Data.IntMap.Lazy.IntMap Int--victories by sID
} deriving (Show)

data SimState =
	forall state. SimState {
		state :: state,
		gameFunc :: [Agent] -> state -> (StatUpdate, state)
	}--show?

update (SimState s f) agents = (second (flip SimState f)) (f agents s)

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

--run :: Simulation -> Simulation
--run s =
--	let
--		--func = gameFunc sim s (state sim s)
--		--diff = (gameFunc  (sim s) $ sim s) $ agents s state
--		--diff = (gameFunc  $ (sim s)) (agents s) (state $ (sim s))
--	in
--		Simulation{
--			agents = agents s,
--			stats = stats s--statMerge diff $ stats s,
--			sim = sim s
--		}

--step s = Simulation( foldToStats[(recordStats a b)| a <-
--s.Agentslist, b <- s.Agentslist, a != b],
--[getEvolved of A | A<- (s.Agentlist), A needs evolution])
--multiple evolutions??
