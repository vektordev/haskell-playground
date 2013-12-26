module CommonStatistics where


type Memory = [Int]--Internal state of an Agent.

data StatUpdate = StatUpdate{
	newVictories :: [Int] -- by agentID
} deriving (Show)

data Agent = Agent{
	agentID :: Int,

	sourcePath :: FilePath, --path to .hs source file. relative to TODO

	doFunc :: [String] -> Memory -> (Memory, [String]),
	--ideally tries to solve a problem at hand.
	--Param: Description of the surrounding world
	--Returns: List of actions the agent wish performs

	evFunc :: [String] -> Memory -> (Memory, String),
	--ideally recombinates a list of .hs files (given as Strings) into a new .hs file
	--to keep this function pure, .hs files are provided content-only.

	personalMemory :: Memory
}

instance Show Agent where
	show (Agent {
		agentID = aID,
		sourcePath = path,
		doFunc = forgetit,
		evFunc = forgetittoo,
		personalMemory = mem
	}) = show (aID, path, mem)
