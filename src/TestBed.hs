import Debug.Trace
import System.Random
import Control.Monad
import Data.Ratio
import Data.Maybe
import Data.List
import System.IO
import Data.Number.CReal
--import Graphics.Vty.Widgets.All
--import UI.NCurses

--main = putStrLn (show $ tailfib 0 1 100000)

import qualified Data.Text as T

main :: IO()
main = print prefix2stats

--main :: IO ()
--main = do
--	e <- editWidget
--	ui <- centered e
--	fg <- newFocusGroup
--	addToFocusGroup fg e
--
--	c <- newCollection
--	addToCollection c ui fg
--	e `onActivate` \this ->
--		getEditText this >>= (error . ("You entered: " ++) T.unpack)
--	runUi c defaultContext

--graph isomery? (x, y) is edge, x,y Int
--(x,y) => no other (x,y) or (y,x) exists
--interconnectivity: for any x,y -> chain x, a, b, ... y exists

--x `isomer` y
--edgeCount == edgeCount
--edgeCount for each vertex is a permutation of the other graph's
--try renaming every node of x to match every name in y. Abort if edge count doesn't match. If doesn't work, not isomer

term :: Int -> [String]
term 0 = [""]
--term 1 = ["()"]
term n = ["(" ++ smallterm ++ ")" | smallterm <- term (n-1)] ++ [terma ++ termb | x <- [1..n], y <- [1..n], x+y == n, terma <- term x, termb <- term y]

term' :: Int -> [String]
term' n = termh n 0 ""

termh :: Int -> Int -> String -> [String]
termh 0 0 str = [str]
termh 0 y str = termh 0 (y-1) (str ++ ")")
termh x 0 str = termh (x-1) 1 (str ++ "(")
termh x y str = (termh x (y-1) (str ++ ")")) ++ (termh (x-1) (y+1) (str ++ "("))

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib(n-2)

data Automaton = Automaton{
	states :: [Int],
	functions :: [(Int, Char, Int)],
	start :: [Int],
	end :: [Int]
}

first (a, _, _) = a
second (_, a, _) = a
third (_, _, a) = a

assertDEA :: Automaton -> Bool
assertDEA automaton =
	let
		statesused = (map first $ functions automaton) ++ (map third $ functions automaton) ++ start automaton ++ end automaton
		isValid = foldl (\acc state -> acc && (elem state $ states automaton)) True statesused
		isDet = foldl (\acc grp -> acc && length grp == 1) True $ groupBy (\(a, b, _) (d, e, _) -> a == d && b == e) $ functions automaton
	in  isValid && isDet

aabSTARAutomaton = Automaton{
	states = [0, 1, 2],
	start = [0],
	functions = [
		(0, 'a', 1),
		(1, 'a', 2),
		(2, 'b', 0)
	],
	end = [0]
}

abSTARaabAutomaton =  Automaton{
	states = [0,1,2,3],
	start = [0],
	functions = [
		(0, 'a', 0),
		(0, 'b', 0),
		(0, 'a', 1),
		(1, 'a', 2),
		(2, 'b', 3)
	],
	end = [3]
}

abSTARaababSTARAutomaton = Automaton{
	states = [0,1,2,4,5,6],
	start = [0],
	functions = [
		(0, 'a', 2),
		(0, 'a', 1),
		(1, 'b', 0),
		(2, 'a', 4),
		(4, 'b', 5),
		(5, 'a', 6),
		(6, 'b', 5)
	],
	end = [5]
}

data TuringMemory a = TuringMemory {
	left :: [a],
	mid :: a,
	right :: [a]
} deriving (Show)

data TuringMachine a = TuringMachine{
	statesTM :: [Int],
	--alphabets
	inputSymbols :: [a],
	startTM :: Int,
	endTM :: Int,
	delta :: [(Int, a, Int, a, MoveDir)]
}

data MoveDir = MoveLeft | Neutral | MoveRight

compute :: (Eq a) => TuringMachine a -> TuringMemory a -> Int -> TuringMemory a
compute (TuringMachine validStates inp start end deltas) tm state =
	if state == end
	then tm
	else
		let (_, _, newState, newSymb, direction) =fromJust $ find (\(from, symb, _, _, _) -> from == state && symb == mid tm) deltas
		in compute
			(TuringMachine validStates inp start end deltas)
			(move (write tm newSymb) direction)
			newState

write :: TuringMemory a -> a -> TuringMemory a
write (TuringMemory l m r) a = TuringMemory l a r

moveLeft ::  TuringMemory a -> TuringMemory a
moveLeft (TuringMemory l m r) = TuringMemory (tail l) (head l) (m:r)

moveRight ::  TuringMemory a -> TuringMemory a
moveRight (TuringMemory l m r) = TuringMemory (m:l) (head r) (tail r)

move :: TuringMemory a -> MoveDir -> TuringMemory a
move m Neutral = m
move m MoveLeft = moveLeft m
move m MoveRight = moveRight m 


--concat :: Automaton -> Automaton -> Automaton
--toDEA :: Automaton -> Automaton
--minimize :: Automaton -> Automaton

mindfuckAutomaton = Automaton{
	states = [0,1,2,3],
	start = [0],
	functions = [
		(0, 'a', 0),
		(0, 'a', 1),
		(1, 'a', 0),
		(1, 'a', 1)
	],
	end = [1]
}

keepFalsePaths = False

automatonRead :: Automaton -> String -> [(Bool, Int, String, [Int])]
automatonRead automaton input =
	filter
	(\(success, _, _, _) -> keepFalsePaths || success)
	(concat (map (\st -> automatonReadH automaton st input []) $ start automaton))

automatonReadH :: Automaton -> Int -> String -> [Int] -> [(Bool, Int, String, [Int])]
automatonReadH automaton state [] trace  = if elem state $ end automaton then [(True, state, "", trace ++ [state])] else [(False, state, "", trace ++ [state])]
automatonReadH automaton state (symb:rest) trace =
	let tgtStates = map third $ filter (\(from, symb2, to) -> symb2 == symb && state == from) $ functions automaton
	in
		if null tgtStates
		then [(False, state, symb:rest, trace ++ [state])]
		else
			filter
			(\(success, state, restStr, trc) -> keepFalsePaths || success)
			$ concat $ map (\toState -> automatonReadH automaton toState rest (trace ++ [state])) tgtStates

tailfib _ y 0 = y
tailfib x y n = tailfib y (x+y) (n-1)

fibseqcutoff :: Integral a => [a]
fibseqcutoff = 
	let help a b = (mod (a+b) 10000000000000000000000000000000000000000^2): help b (a+b)
	in help 0 1
	
fibseq :: Integral a => [a]
fibseq = 
	let help a b = (a+b): help b (a+b)
	in help 0 1
	
trifibseq :: [CReal]
trifibseq =
	let help a b c = (a+b+c) : help b c (a+b+c)
	in help 0 0 1
	
sequenceRatio :: [CReal] -> [CReal]
sequenceRatio (x : y : xs) = (y / x) : sequenceRatio (y:xs)
sequenceRatio x = []

test = sequenceRatio trifibseq
	
getFirstDigit n = if n >= 10 then getFirstDigit (div n 10) else n

first2LettersOfFibo = map (first2ofStr . show) fibseq

first2ofStr :: String -> String
first2ofStr str = if null $ tail $ str then (take 1 str) else take 2 str

prefix2stats = map (\grp -> (length grp, head grp)) $ group $ sort $ take 10000 $ drop 4 first2LettersOfFibo

firstLettersOfFibo = map (head . show) fibseq

fibStr :: String
fibStr = concatMap (\n -> show n) fibseq
	
prefix :: String -> Char -> Int
prefix [] _ = 0
prefix (char:str) c = if c /= char then 0 else 1 + (prefix str char)

prefAll :: String -> [(Int, Int, Char)]
prefAll [] = []
prefAll str =
	let func str n = ((prefix str $head str), n, head str) : (func (tail str) (n+1))
	in func str 0

prefixFibo minPrefLen = filter (\(n, _, _) -> n >= minPrefLen) $ prefAll fibStr

divs n = [x | x <- [1..n], mod n x == 0]
divsum n = sum $ divs n
divquot n = (divsum n) % n
isperf n = (denominator ((divquot n) + 1%2)) == 1
findperfs m n = [ x | x <- [m..n], isperf x]

myFunc :: Int -> [Char]
myFunc 1 = "one"
myFunc 2 = "two"
myFunc x = "stuff"

sieve n = foldl
	(\primes divisor ->
		if isJust $ find (== divisor) primes
		then foldl (\list rem -> delete rem list) primes [divisor * n | n <- [2..(div n divisor)]]
		else primes)
	[2..n]
	[2..(div n 2)]

sortfile path = do
	string <- readFile path
	return (selectionsort (filter (`elem` ['0'..'~']) string))

selectionsort [] = []
selectionsort list =
	let (smallest, rest) = getSmallest list
	in smallest : selectionsort rest

--getSmallest :: Ord a => [a] -> (a, [a])
getSmallest list = if (length list) >= 2
	then
		let (restsmall,restlist) = getSmallest $ tail list
		in if restsmall <= (head list) then (restsmall, (head list):restlist) else (head list, tail list)
	else
		(head list, [])

someFunction :: Int -> Int
someFunction x = 2 * x

b :: Int -> Int -> Int
b 0 0 = 1
b n 0 = 1 + b (n-1) 1
b 0 m = 1 + b 0 (m-1)
b n m = 1 + b (m-1) (n-1)

--fullack :: Int -> [[Int]]
--damn, I need array-style memory for this...

--fullack :: [[Int]] -> Int -> Int -> [[Int]]
--fullack :: xs n m
--	| (length xs) < n = fullack ([]:xs) n m
--	| n == 0 = (fullack xs n-1 0)

ack :: Int -> Int -> Int
ack 0 m = m + 1
ack n 0 = (ack (n-1) 1)
ack n m = ack (n-1) (ack n (m-1 ) )

fack :: Int -> Int -> Int --fast ack
fack x y = let (a,b) = insertfack x y [] in b

insertlist :: Int -> Int -> [Int] -> [Int]
insertlist value 0 (x:xs) = (value:xs)
insertlist value 0 [] = [value]
insertlist value pos (x:xs) = x:(insertlist value (pos-1) xs)
--insertlist v p x = x

insert2dlist :: Int -> Int -> Int -> [[Int]] -> [[Int]]
insert2dlist value 0 0 [] = [[value]]
insert2dlist value 0 y [] = [] : insert2dlist value 0 (y-1) []
-- errorinsert2dlist value x y [] =  if 10 :: [] then [[]] else [[1]]
insert2dlist value 0 y (list:listr) = (insertlist value y list) : listr
--insert2dlist value x y 
insert2dlist value x y (list:listr) = list:(insert2dlist value (x-1) y listr)
--insert2dlist value x y [list]

getlist :: Int -> [Int] -> Maybe Int
getlist 0 [x] = Just x
getlist y [] = Nothing
getlist y (x:xs) = getlist (y-1) xs

get2dlist :: Int -> Int -> [[Int]] -> Maybe Int
get2dlist 0 y (list:listr) = getlist y list
get2dlist x y (list:listr) = get2dlist (x-1) y listr
get2dlist x y a = Nothing

insertfack :: Int -> Int -> [[Int]] -> ([[Int]],Int)
insertfack x y list =
	let getval = get2dlist x y list
	in if isJust getval
	then (list,fromJust getval)
	else insert2fack x y list

insert2fack :: Int -> Int -> [[Int]] -> ([[Int]], Int)
insert2fack 0 0 list = (insert2dlist 1 0 0 list, 1)
insert2fack 0 y list =
	let (updlist, updint) = insertfack 0 (y-1) list
	in ((insert2dlist (y+1) 0 y updlist), y+1)
insert2fack x 0 list =
	let (updlist, updint) = insertfack (x-1) 1 list
	in (insert2dlist updint x 0 updlist, updint)
insert2fack x y list =
	let
		(updlist, updint) = insertfack x (y-1) list
		(upd2list, upd2int) = insertfack (x-1) updint updlist
	in
		(insert2dlist upd2int x y upd2list,upd2int)

--insertfack :: Int -> Int -> [[Int]] -> [[Int]]
--insertfack 0 0 (list:listsrest) = ((insertlist 1 0 list):listsrest)
--insertfack 0 b (list:listsrest) =
--	let (filledlist:filledlistrest) = insertfack 0 (b-1) (list:listsrest)
--	in ((insertlist (b+1) b filledlist):filledlistrest)
--insertfack a 0 (list:listsrest) =
--	let (filledlist:filledlistrest) = insertfack (a-1) 1 (list:listsrest)
--	in (insert2dlist (get2dlist (a-1) 1 (filledlist:filledlistrest)) a 0 (filledlist:filledlistrest))
--insertfack a b (list:listsrest) =
--	let (filledlist:filledlistrest) = insertfack a (b-1) (list:listsrest)
--	let (filledlist2:filledlistrest2) = insertfack (a-1) (get2dlist  a (b-1) (filledlist:filledlistrest)) (filledlist:filledlistrest)
--	in (insert2dlist (get2dlist (a-1) 1 (filledlist:filledlistrest)) a 0 (filledlist:filledlistrest))




fact :: Int -> Int
fact 1 = 1
fact n = n * fact (n-1)

--fibo :: Int -> [Int] -> [Int]
fibo 0 x = x
fibo n [] = fibo (n-1) [1]
fibo 1 (x:y:xs) = x+y:x:y:xs
fibo n (x:[]) = fibo (n-1) (1:[x])
fibo n xs =
	let ys = fibo (n-1) (xs)
	in ((head ys) + head (tail ys)):ys	


primesBinInterval :: Int -> Int -> [Int]
primesBinInterval x y = [primesInInterval (2^intervalID) 2 | intervalID <- [x..y]]

primesInInterval x y = length (primes3 x (y*x))

primes 1 = []
primes n = if(prime n)
	then n : (primes (n-1))
	else primes (n-1)
	
primes' n = take n $ filter prime [1..]

primes2 :: Int -> [Int]
primes2 n = [primes | primes <- [1..n], prime primes]

primes3 1 1 = []
primes3 n m = if(n > m)
	then []
	else if(prime n)
		then n : primes3 (n+1) m
		else primes3 (n+1) m

prime :: Int -> Bool
prime x = if (x < 2)
	then False
	else null [y | y <- [2..floor $ sqrt $ fromIntegral x], mod x y == 0]

testData elems = replicateM elems randomIO

testSort n = liftM mergesort $ replicateM n randomIO

testSort2 n = liftM quicksort $ replicateM n randomIO

parti :: (Ord a) => a -> [a] -> ([a], [a])
parti pivot lst = (filter (<= pivot) lst, filter (>pivot) lst)

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort lst =
	let (front, back) = parti (head lst) $ tail lst
	in (quicksort front) ++ head lst : (quicksort back)

shortsort x = if length x == 0 then [] else shortsort (filter (<= head x) $ tail x) ++ (head x : shortsort (filter (> head x) $ tail x))

ss x = let f=filter;h=head x;t=tail x in if x==[] then [] else ss (f (<= h) $ t) ++ (h : ss (f (> h) t))

mergesort :: (Ord a) => [a] -> [a]
mergesort from =
	let
		lengthfrom = (length from)
		halflength = (lengthfrom `div` 2)
	in if (lengthfrom > 1)
	then
		merge2
			(mergesort (take halflength from))
			(mergesort (drop halflength from))
			--[]
	else from

merge :: (Ord a) =>[a] -> [a] -> [a] -> [a]
merge [] [] target = target
merge fromA [] target = target ++ fromA
merge [] fromB target = target ++ fromB
merge fromA fromB target = if ( (head fromA) < (head fromB) )
	then
		merge (tail fromA) (fromB) (target ++ [(head fromA)] )
	else
		merge (fromA) (tail fromB) (target ++ [(head fromB)] )
		
merge2 :: (Ord a) =>[a] -> [a] -> [a]
merge2 fromA [] = fromA
merge2 [] fromB = fromB
merge2 fromA fromB = if ( (head fromA) < (head fromB) )
	then
		head fromA : merge2 (tail fromA) (fromB)
	else
		head fromB : merge2 (fromA) (tail fromB)

type Coordinates = (Int, Int)

data Direction = Left | Up | Right | Down

data GameField = Array Coordinates Int

--step :: GameField  -> Direction -> GameField
--step field dir = 
