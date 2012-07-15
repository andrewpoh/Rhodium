{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Rhodium.Segment.Segments
	where

import qualified Data.Vector.Unboxed as V
import Data.List
import qualified Data.Map as M
import Rhodium.Data.Dataframe
import Rhodium.Data.DataColumn
import Rhodium.Data.DataCell
import Rhodium.Segment.Discretisers
import Rhodium.Segment.Matchers
import Rhodium.Segment.Aggregators

-- Split creators should be unfolds

doubleSplits :: Dataframe -> Int -> Int -> Name -> [Int] -> [DoubleSplit]
doubleSplits frame minSize minStep doubleColName indices =
	let rowCount = length indices in
	let doubleArray = getDoubleColumn frame doubleColName in
	let elements = map ((V.!) doubleArray) indices in
	let sorted = (V.fromList . sort) elements in
	let op = splitDoubles_ doubleColName minSize minStep sorted rowCount in
	unfoldr op minSize

-- should check for duplicates
-- totalf should match length of array
splitDoubles_ :: Name -> Int -> Int -> V.Vector Double
	-> Int -> Int
	-> Maybe (DoubleSplit, Int)
splitDoubles_ columnName minSize minStep sortedArray totalf index =
	let remf = totalf - index in
	if remf < minSize
		then Nothing
		else
			let breakpoint = sortedArray V.! index in
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
intSplits :: Dataframe -> Int -> Int -> Name -> [Int] -> [IntSplit]
intSplits frame minSize minStep intColName indices =
	let histogram = aggregate (IntDisc intColName) CountAgg frame indices in
	let histogramList = M.assocs histogram in
	let totalFrequencies = (sum . map snd) histogramList in
	let initialFreq = snd $ head histogramList in
	let thist = tail histogramList in
	let op = splitInts_ minSize minStep (length indices) intColName in
	if length histogramList < 2 then []
		else snd $ foldl op ((initialFreq, initialFreq), []) thist

splitInts_ ::
	Int -> Int -> Int -> Name
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
aggregate :: (Ord l, Discretiser k l, Aggregator g) =>
	k -> g -> Dataframe -> [Int] -> M.Map l (AggResult g)
aggregate disc agg frame indices =
	M.map (partialToFinal agg) (foldl' adjust M.empty indices)
	where
	adjust store index =
		let merge n o = mergePartials agg [n, o] in
		let discretise = discretiseSingle disc frame index in
		let process = processSingle agg frame index in
		M.insertWith' merge discretise process store
