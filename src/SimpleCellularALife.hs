module SimpleCellularALife where

import Data.Array
import Data.List
import System.Random
import CommonStatistics

data Ground = Ground {
	food :: Int
} deriving (Show)

data Entity = Entity{
	ai :: Agent, 
	health :: Int
} deriving (Show)

data Tile = Tile{
	ground :: Ground,
	entities :: [Entity]
} deriving (Show)

data World = World {
	tiles :: Array Coordinates Tile
} deriving (Show)

type Coordinates = (Int, Int)

--scalFunc :: [Agent] -> World -> String -> IO (StatUpdate, [Agent], World)
--place Agents
--run Simulation

getRandomEmptyTile :: RandomGen t => t -> (Tile, t)
getRandomEmptyTile g =
	let (i, g2) = next g
	in
		(Tile{
			ground = Ground{food = mod i 6},
			entities = []},
		g2)

getInitialState :: RandomGen t => Int -> Int -> t -> (t, World)
getInitialState x y r =
	let (r2, randtiles) =
		(foldl
			(\(rnd, lst) _ -> let (tile, newRnd) = getRandomEmptyTile rnd in (newRnd, tile : lst))
			(r,[])
			[() | a <- [0..x], b <- [0..y]])
	in (r2, World{
		tiles = listArray ((0, 0), (x, y)) randtiles
	})

getRandomPosition :: RandomGen t => World -> t -> (t, Coordinates)
getRandomPosition w rand =
	let
		((minx, miny), (maxx, maxy)) = bounds $ tiles w
		(x, rand2) = randomR (minx, maxx) rand
		(y, rand3) = randomR (miny, maxy) rand2
	in
		(rand3, (x, y))

defaultHealth = 100

placeAgents :: RandomGen t => World -> [Agent] -> t -> (t, World)
placeAgents w agents rand =
	let
		createTileUpdates (ag:ags) wrld rnd =
			let
				(rnd2, coords) = getRandomPosition wrld rnd
				(rnd3, restUpd) = createTileUpdates ags wrld rnd2
			in (rnd3, (coords, Entity{health = defaultHealth, ai = ag}) : restUpd)
		createTileUpdates [] wrld rnd = (rnd, [])
		
		(rand2, assocList) = createTileUpdates agents w rand
		groupedAssocList = groupBy (\(c1, a1) (c2, a2) -> c1 == c2) $ sortBy (\(c1, a1) (c2, a2) -> compare c1 c2) assocList
		gnd grp = ground ((tiles w) ! (fst $ head grp))
		condenseGroup grp = (fst $ head grp, Tile{ground = gnd grp, entities = snd $ unzip grp})
		worldupdate = map condenseGroup groupedAssocList -- ::(Coordinates, Tile)
	in
		(rand2, World{tiles = (tiles w) // worldupdate})


--Effect of non-continuous world on group behavior? In contrast to real world, Agents can not necessarily rely on their group in all situations. Is that good or bad?

--World parameter to scalFunc mustn't contain Agents - use emptyWorld as state instead. Why even bother with state given existance of AgentSetControls? As an input, it's an important part of the simulation initialization, the returned value can be mined for information.

