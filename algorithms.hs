module Algorithms
	where

import Control.Monad.ST
import Data.Array.IArray
import Data.Array.ST
import Data.STRef
import Random

permutation :: (RandomGen g, IArray ia a) => g -> ia Int a -> (g, [a])
permutation g a =
	let (down,up) = bounds a in
	let size = up-down+1 in
	let (g1, indices) = shuffle g size in
	(g1, map ((!) a) indices)

shuffle :: RandomGen g => g -> Int -> (g, [Int])
shuffle g n = runST (shuffle_ g n)

shuffle_ :: RandomGen g => g -> Int -> ST s (g, [Int])
shuffle_ g n = do
	let nTakeOne = n-1
	let arrayBounds = (0, nTakeOne)
	array <- newListArray arrayBounds [0..nTakeOne]
	cell <- newSTRef g
	shuffleR_ array cell nTakeOne

shuffleR_ :: RandomGen g => STArray s Int Int -> STRef s g -> Int
	-> ST s (g, [Int])
shuffleR_ array cell 0 = do
	g <- readSTRef cell
	list <- getElems array
	return (g, list)
shuffleR_ array cell t = do
	g <- readSTRef cell
	let (otherIndex, g1) = randomR (0, t) g
	writeSTRef cell g1
	currentItem <- readArray array t
	otherItem <- readArray array otherIndex
	writeArray array otherIndex currentItem
	writeArray array t otherItem
	shuffleR_ array cell (t-1)
