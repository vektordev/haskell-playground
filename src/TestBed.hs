import Debug.Trace
import System.Random
import Control.Monad
import Data.Ratio
import Data.Maybe

main = putStrLn (show $ tailfib 0 1 100000)

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib(n-2)

tailfib _ y 0 = y
tailfib x y n = tailfib y (x+y) (n-1)

divs n = [x | x <- [1..n], mod n x == 0]
divsum n = sum $ divs n
divquot n = (divsum n) % n
isperf n = (denominator ((divquot n) + 1%2)) == 1
findperfs m n = [ x | x <- [m..n], isperf x]

myFunc :: Int -> [Char]
myFunc 1 = "one"
myFunc 2 = "two"
myFunc x = "stuff"

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
		

