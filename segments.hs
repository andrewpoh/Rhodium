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

intSplits :: Dataframe -> Int -> Int -> String -> [IntSplit]
intSplits frame minSize minStep intColName =
	let histogram = M.assocs $ aggregate (IntDisc intColName) CountAgg frame in
	let totalFrequencies = (sum . map snd) histogram in
	let initialFreq = snd $ head histogram in
	let thist = tail histogram in
	let op = chooseIntSplit_ minSize minStep (getRowCount frame) intColName in
	if length histogram < 2 then []
		else snd $ foldl op ((initialFreq, initialFreq), []) thist

chooseIntSplit_ ::
	Int -> Int -> Int -> String
	-> ((Int, Int), [IntSplit]) -> (Int, Int)
	-> ((Int, Int), [IntSplit])
chooseIntSplit_ minSize minStep totalf columnName
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
