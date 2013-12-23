module RuntimeCompiler where

import CommonStatistics

compile :: FilePath -> IO(
	[String] -> Memory -> (Memory, [String]),--goes by the name of doFunc
	[String] -> Memory -> (Memory, String))--goes by the name of evFunc

compile a = return ((\ a b -> (b,a)),(\a b -> (b, head a)))
