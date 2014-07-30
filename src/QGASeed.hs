{-# LANGUAGE Safe #-}
--ALWAYS compile results of this as Safe

import System.Random
import Data.List (isInfixOf)
import System.Environment (getArgs)
import System.Process (readProcessWithExitCode)
import Data.Maybe (isJust, fromJust)
import System.Exit (ExitCode(..))

--main = putStr (s ++ "; s = " ++ show s);
--s = "main = putStr (s ++ \"; s = \" ++ show s)"

main = do
    args <- getArgs
    generateOffspring 0 0 (read $ (args !! 1)) $head args 
    --case args of  
        --[path] -> do
            --handle <- openFile (path ++ subset ++ (show id) ++ "Statistics") WriteMode
            --hPutStr stathandle $ getNewStats id parentID
            --hClose stathandle
            --writeFile path++'-'['.','h','s']
            --write newlygen sourcecode
            --try to compile readProcess "ghc" [path] ""
            --on failure, retry
            --if it works, fork out process.
        --_ -> putStrLn "nope. Need a file path"

--main: write best shot to given location

generateOffspring :: Int -> Int -> Int -> String -> IO()
generateOffspring _ 100 _ _ = return () --max attempts: set here
generateOffspring 10 _ _ _ = return () --max offspring: set here
generateOffspring successNum failNum depth path = do
    rndGen <- newStdGen
    baseSrc <- readFile (path ++ ['.','h','s'])
    src <- reprogram [rndGen] [baseSrc]
    if isJust src
    then do
        writeFile (path ++ '-' : (show successNum) ++ ['.','h','s']) $ fromJust src
        --try compiling
        (code, out, err) <- readProcessWithExitCode ['g','h','c'] [path ++ '-' : (show successNum) ++ ['.','h','s']] []
        --Fork, decrease depth
		spawnCommand '.' : '/' : path ++ '-' : (show successNum)
        if code == ExitSuccess
        then generateOffspring (successNum+1) failNum depth path
        else generateOffspring successNum (failNum+1) depth  path
    else
        generateOffspring successNum (failNum+1) depth  path

reprogram :: [StdGen] -> [String] -> IO (Maybe String) --only ever attempts, that's as good as it gets.
reprogram rngs sources = do
    let proposedCode = code rngs sources
    unique <- isUnique (head sources) proposedCode
    if unique && not (hasImports proposedCode)
    then return $ Just (prepend ++ proposedCode)
    else return Nothing

--no imports
hasImports :: String -> Bool
hasImports source = isInfixOf "import" source || isInfixOf "{-#" source || isInfixOf "#-}" source --prevents import in any context, even when part of a identifier

--uniqueness??
isUnique :: String -> String -> IO (Bool)
isUnique ss r = return (r == ss)
--this needs more work. Should hash every program or so, not sure yet.

prepend = "enter above code as String"--TODO

code :: [StdGen] -> [String] -> String
code rnd s = head s ++ ('\n':'-':'-': (show $ fst $ next $ head rnd)) --start here: implement

--here be shenanigans
