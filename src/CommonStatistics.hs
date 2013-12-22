module CommonStatistics where

--data Memory = Memory{ --an Agent's internal memory
--	stuffs :: [Int]
--} deriving (Show)
type Memory = [Int]

data StatUpdate = StatUpdate{
	newVictories :: [Int]
} deriving (Show)

data Agent = Agent{
	agentID :: Int,
	sourcePath :: FilePath,
	doFunc :: [String] -> Memory -> (Memory, [String]),
	evFunc :: [String] -> Memory -> (Memory, String),
	personalMemory :: Memory
}

instance Show Agent where
	show (Agent {agentID = aID, sourcePath = path, doFunc = forgetit, evFunc = forgetittoo, personalMemory = mem}) = show (aID, path, mem)
