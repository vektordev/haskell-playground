module HsCodeGen where

import System.Random

generateSourceFile :: [String] -> StdGen -> (String, StdGen)
generateSourceFile sourcefiles rnd =
	let	prepoc = ""
		imports = ""
		datadecl = ""
		funcHeaders = unlines ["evFunc :: [String] -> Memory -> (Memory, String)",""]
		funcImpl = implement funcHeaders rnd
	in (prepoc ++ imports ++ datadecl ++ funcHeaders ++ (fst funcImpl), snd funcImpl)
	
implement :: String -> StdGen -> (String, StdGen)
implement headers rnd = foldl (\(str, r) fhead -> let (r2, impl) = implementOneFct fhead r in (impl ++ str, r2)) ("", rnd) (lines headers)
--if backwards, swap impl ++ str with str ++ impl

implementOneFct :: String -> StdGen -> (StdGen, String)
implementOneFct str r = (r, str)
