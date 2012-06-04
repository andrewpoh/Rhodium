module Algorithms
	where

import Control.Monad.ST
import qualified Data.Array as A
import Data.Array.IArray
import Data.Array.ST
import Data.STRef
import System.Random

sampleReplaceList :: RandomGen g => [a] -> Int -> g -> (g, [a])
sampleReplaceList l n g =
	let b = (0, length l - 1) in
	let arr = A.listArray b l in
	sampleReplace arr n g

sampleReplace :: (RandomGen g, IArray ia a) =>
	ia Int a -> Int -> g -> (g, [a])
sampleReplace a n g =
	let (gx, ixs) = randomList (bounds a) n g in
	(gx, map ((!) a) ixs)

randomList :: RandomGen g => (Int, Int) -> Int -> g -> (g, [Int])
randomList b n g =
	if n < 1
		then (g, [])
	else
		let (x, g1) = randomR b g in
		let (gx, xs) = randomList b (n-1) g1 in
		(gx, x:xs)

permutationList :: RandomGen g => g -> [a] -> (g, [a])
permutationList g l =
	let b = (0, length l - 1) in
	let arr = A.listArray b l in
	permutation g arr
		
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
