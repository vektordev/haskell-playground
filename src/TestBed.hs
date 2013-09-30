import Debug.Trace
import System.Random
import Control.Monad

main = putStrLn (show (ack 3 3))

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
		

