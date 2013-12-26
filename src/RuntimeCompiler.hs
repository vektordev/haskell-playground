module RuntimeCompiler where

import CommonStatistics
--because we need to know Memory

--requires a FilePath to a .hs file.
--From that .hs file, this function compiles the 2 functions evFunc and doFunc.
--This function needs to ensure that both functions evFunc and doFunc returned
--are actually pure. Also, compile should itself not expose any security
--problems. It should generally be assumed that the code in the provided file is
--very evil.
compile :: FilePath -> IO(
	[String] -> Memory -> (Memory, [String]),--goes by the name of doFunc
	[String] -> Memory -> (Memory, String))--goes by the name of evFunc

compile a = return ((\ a b -> (b,a)),(\a b -> (b, head a)))

