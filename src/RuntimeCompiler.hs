module RuntimeCompiler where

-- Note: Look at safe-plugins for potential way to use Safe Haskell in the
-- plugin loader
import System.Plugins.Load
import Control.Monad.Trans.Except
import Control.Monad.IO.Class

import CommonStatistics
--because we need to know Memory

type DoFunc = [String] -> Memory -> (Memory, [String])
type EvFunc = [String] -> Memory -> (Memory, String)

loadExc :: FilePath -> [FilePath] -> Symbol -> ExceptT String IO (Module, a)
loadExc obj inc conf sym = liftIO (load_ obj inc sym) >>= (\x->case x of
	LoadSuccess x	-> return x
	LoadFailure s	-> throwE s)

reloadExc :: Module -> Symbol -> ExceptT String IO (Module, a)
reloadExc mod sym = liftIO (reload mod sym) >>= (\x->case x of
	LoadSuccess x	-> return x
	LoadFailure s	-> throwE s)

-- requires a FilePath to a .hs file.
-- From that .hs file, this function compiles the 2 functions evFunc and doFunc.
-- This function needs to ensure that both functions evFunc and doFunc returned
-- are actually pure. It should be safe to compile untrusted code.
compile :: FilePath -> IO (Either String (DoFunc, EvFunc))
compile pth = runExceptT $ do
	-- Try compiling the source file
	mod,ev <- loadExc pth [] "evFunc"
	_,dof <- reloadExc mod "doFunc"
	return (dof,ev)
