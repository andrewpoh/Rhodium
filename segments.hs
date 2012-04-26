{-# OPTIONS_GHC -XTypeFamilies #-}
{-# OPTIONS_GHC -XFlexibleContexts #-}
module Segments
	where

import qualified Data.Array.Unboxed as A
import Data.List
import qualified Data.Map as M
import Aggregators
import Dataframe
import DataColumn
import DataCell
import Discretisers
import Matchers

-- Split creators should be unfolds

doubleSplits :: Dataframe -> Int -> Int -> String -> [DoubleSplit]
doubleSplits frame minSize minStep doubleColName =
	let rowCount = getRowCount frame in
	let doubleArray = getDoubleColumn frame doubleColName in
	let sorted =
		(A.listArray (0, rowCount-1) . sort . A.elems) doubleArray in
	let op = splitDoubles_ doubleColName minSize minStep sorted rowCount in
	unfoldr op minSize

-- should check for duplicates
-- totalf should match length of array
splitDoubles_ :: Name -> Int -> Int -> A.UArray Int Double
	-> Int -> Int
	-> Maybe (DoubleSplit, Int)
splitDoubles_ columnName minSize minStep sortedArray totalf index =
	let remf = totalf - index in
	if remf < minSize
		then Nothing
		else
			let breakpoint = sortedArray A.! index in
			Just (DoubleSplit (columnName, breakpoint), index+minStep)
{-
 - Should implement unfold version
intSplits2 :: Dataframe -> Int -> Int -> String -> [IntSplit]
intSplits2 frame minSize minStep intColName =
	let columnArray = getintColumn frame intColName in
	let intDisc = IntDisc intColName in
	let histogram = M.assocs $ aggregate intDisc CountAgg frame in
	unfoldr 
-}
intSplits :: Dataframe -> Int -> Int -> String -> [IntSplit]
intSplits frame minSize minStep intColName =
	let histogram = M.assocs $ aggregate (IntDisc intColName) CountAgg frame in
	let totalFrequencies = (sum . map snd) histogram in
	let initialFreq = snd $ head histogram in
	let thist = tail histogram in
	let op = splitInts_ minSize minStep (getRowCount frame) intColName in
	if length histogram < 2 then []
		else snd $ foldl op ((initialFreq, initialFreq), []) thist

splitInts_ ::
	Int -> Int -> Int -> String
	-> ((Int, Int), [IntSplit]) -> (Int, Int)
	-> ((Int, Int), [IntSplit])
splitInts_ minSize minStep totalf columnName
	((cumf, stepf), splits) (key, nextf) =
	let remf = totalf - cumf in
	let cumf1 = cumf+nextf in
	let stepf1 = stepf+nextf in
	let skipElement = ((cumf1, stepf1), splits) in
	let newMatcher = IntSplit (columnName, key) in
	let keepElement = ((cumf1, nextf), newMatcher:splits) in
	if cumf < minSize || remf < minSize || stepf < minStep
		then skipElement
		else keepElement

-- This requires Flexible Contexts, which might be too much
aggregate :: (Ord (DiscType k), Discretiser k, Aggregator g) =>
	k -> g -> Dataframe -> M.Map (DiscType k) (AggResult g)
aggregate disc agg frame =
	M.map (partialToFinal agg) (foldl' adjust M.empty indices)
	where
	indices = [0..(getRowCount frame-1)]
	adjust store index =
		M.insertWith' (\n o -> mergePartials agg [n, o])
			(discretiseSingle disc frame index) (processSingle agg frame index) store
