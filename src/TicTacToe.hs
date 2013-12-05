import CommonStatistics
import Control.Applicative
import Data.List
import Data.Maybe

data TTTField =  TTTField (Char, Char, Char) (Char, Char, Char) (Char, Char, Char)

emptyField :: TTTField
emptyField = TTTField(' ',' ',' ')(' ',' ',' ')(' ',' ',' ')

getResponse :: Agent -> TTTField -> (Agent,Int)
getResponse ag (TTTField (a, b, c)(d, e, f)(g, h, i)) =
	let (mem,[actionx, actionxs]) =
		(doFunc ag) [[a,b,c],[d,e,f],[g,h,i]] (personalMemory ag)
	in (Agent{
		agentID = agentID ag,
		sourcePath = sourcePath ag,
		doFunc = doFunc ag,
		evFunc = evFunc ag,
		personalMemory = mem
	}, read actionx)
--getResponse ag emptyField = 0

versus :: Agent -> Agent -> (Int, Agent, Agent)
versus a b =
	let (winner, aag, bag) = turns a b emptyField
	in ((if (winner == 1) then (agentID aag) else (agentID bag)),aag,bag)

turns :: Agent -> Agent -> TTTField -> (Int, Agent, Agent) --either TTTField or Int
turns a b field
	| getWinner afield /= ' ' = (1, aag,b)
	| getWinner bfield /= ' ' = (2, aag, bag)
	| otherwise = turns aag bag bfield
	where
		(afield,aag) = (makeTurn a field 'X')
		(bfield,bag) = (makeTurn b afield 'O')

makeTurn :: Agent -> TTTField -> Char -> (TTTField,Agent)
makeTurn ag (TTTField (a, b, c)(d, e, f)(g, h, i)) symb
	| fieldID <= 2 = (TTTField 
		(case fieldID of
			1 -> (symb, b, c)
			2 -> (a, symb, c)
			3 -> (a, b, symb)
		)
		(d,e,f)
		(g,h,i),ag)
	| fieldID <= 5 = (TTTField
		(a,b,c)
		(case fieldID of
			4 -> (symb, e, f)
			5 -> (d, symb, f)
			6 -> (d, e, symb)
		)
		(g,h,i),ag)
	| otherwise = (TTTField
		(a,b,c)
		(d,e,f)
		(case fieldID of
			7 -> (symb, h, i)
			8 -> (g, symb, i)
			9 -> (g, h, symb)
		),ag)
	where (ag,fieldID) = getResponse ag (TTTField (a, b, c)(d, e, f)(g, h, i)) 

getWinner :: TTTField -> Char
getWinner (TTTField ('X','X','X') _ _) = 'X'
getWinner (TTTField ('O','O','O') _ _) = 'O'
getWinner (TTTField _ ('X','X','X') _) = 'X'
getWinner (TTTField _ ('O','O','O') _) = 'O'
getWinner (TTTField _ _ ('X','X','X')) = 'X'
getWinner (TTTField _ _ ('O','O','O')) = 'O'

getWinner (TTTField ('X',_,_) ('X',_,_) ('X',_,_)) = 'X'
getWinner (TTTField ('O',_,_) ('O',_,_) ('O',_,_)) = 'O'
getWinner (TTTField (_,'X',_) (_,'X',_) (_,'X',_)) = 'X'
getWinner (TTTField (_,'O',_) (_,'O',_) (_,'O',_)) = 'O'
getWinner (TTTField (_,_,'X') (_,_,'X') (_,_,'X')) = 'X'
getWinner (TTTField (_,_,'O') (_,_,'O') (_,_,'O')) = 'O'

getWinner (TTTField ('X',_,_) (_,'X',_) (_,_,'X')) = 'X'
getWinner (TTTField ('O',_,_) (_,'O',_) (_,_,'O')) = 'O'
getWinner (TTTField (_,_,'X') (_,'X',_) ('X',_,_)) = 'X'
getWinner (TTTField (_,_,'O') (_,'O',_) ('O',_,_)) = 'O'
getWinner x = ' '

stuff agents =(,) <$> agents <*> agents

tttHelp :: [Agent] -> (Int, Int) -> ([Agent], Int)
tttHelp agents (agAID,agBID) =
	let
		agA = fromJust $ find (\ agent -> agentID agent == agAID) agents
		agB = fromJust $ find (\ agent -> agentID agent == agBID) agents
		(winner, agA', agB') = versus agA agB
	in
		([
			if (agentID x == agentID agA)
			then agA'
			else (
				if (agentID x ==  agentID agB)
				then agB'
				else x
			) | x <- agents], winner)

tttFunc :: [Agent] -> () -> (StatUpdate, [Agent], ())
tttFunc agents _ =
	let
		agentIDs = [agentID a | a <- agents]
		tuples = (,) <$> agentIDs <*> agentIDs
		(agents, winnerIDs) = mapAccumL (tttHelp) (agents) (tuples)
	in
		(StatUpdate{newVictories = winnerIDs},agents, ())

--tttFunc :: [Agent] -> () -> (StatUpdate, ())
--tttFunc agents _ =
--	(StatUpdate{
--		newVictories =
--			[versus x y | x <- agents, y <- agents,
--				(agentID x) /= (agentID y)]
--	},())
