module Tests.Rhodium.Models.RandomForest
	where

--module DecisionTree
--	where
-- ghc -prof -auto-all -rtsopts DecisionTree.hs
-- +RTS -p

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

import qualified Data.Text as T
import qualified Data.Vector as B
import qualified Data.Vector.Unboxed as V
import Debug.Trace
import Rhodium.Models.DecisionTree
import Rhodium.Models.RandomForest

main :: IO ()
main = do
	doForest

doForest :: IO ()
doForest = do
	forest <- makeForest
	scoreForest forest

doTree :: IO ()
doTree = do
	tree <- makeTree
	scoreTree tree

makeForest :: IO ([DecisionTree Double])
makeForest = do
	rawTable <- readFile "miniTrain.csv"
	let table = readTable rawTable
	let seed = mkStdGen 1337
	let names = tail (getColumnNames table)
	let namesArray = B.fromList names
	let config = RandomTreeConfig 5 10 10 (T.pack "Activity") namesArray
	let allIndices = getIndices table
	let (seed1, trees) = growRandomForest config 50 table allIndices seed
	return trees

scoreForest :: [DecisionTree Double] -> IO ()
scoreForest trees = do
	rawTable <- readFile "miniTest.csv"
	let table = readTable rawTable
	let predictions = runForest trees table
	let response = V.toList (getDoubleColumn table (T.pack "Activity"))
	let treeScore = sum' (zipWith logLoss response predictions) / (fromIntegral (length response))
	print treeScore

scoreTree :: DecisionTree Double -> IO ()
scoreTree tree = do
	rawTable <- readFile "miniTest.csv"
	let table = readTable rawTable
	let predictions = map (runTree table tree) (getIndices table)
	let response = V.toList (getDoubleColumn table (T.pack "Activity"))
	let treeScore = sum' (zipWith logLoss response predictions) / (fromIntegral (length response))
	print treeScore

makeTree :: IO (DecisionTree Double)
makeTree = do
	rawTable <- readFile "miniTrain.csv"
	let table = readTable rawTable
	let seed = mkStdGen 1337
	let names = tail (getColumnNames table)
	let namesArray = B.fromList names
	let config = RandomTreeConfig 5 9 9 (T.pack "Activity") namesArray
	let allIndices = getIndices table
	let (seed1, tree) = growRandomTree config table allIndices seed
	return tree
