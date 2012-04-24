module Segments
	where

import qualified Data.Array as A
import Data.List
import qualified Data.Map as M
import Aggregators
import Dataframe
import DataColumn
import DataCell
import Matchers

data Discretiser k =
	Discretiser (Dataframe -> Int -> k)

simpleDisc :: String -> Discretiser String
simpleDisc columnName =
	Discretiser (\frame index -> showCell $ getCell (getColumn frame columnName) index)

intDisc :: String -> Discretiser Int
intDisc columnName = 
	Discretiser (\frame index -> fromIntCell $ getCell (getColumn frame columnName) index)

intSplits :: Dataframe -> Int -> Int -> String -> [IntSplit]
intSplits frame minSize minStep intColName =
	let histogram = M.assocs $ aggregate (intDisc intColName) CountAgg frame in
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

aggregate :: (Ord k, Aggregator g) => Discretiser k -> g -> Dataframe -> M.Map k (AggResult g)
aggregate (Discretiser makeKey) agg frame =
	M.map (partialToFinal agg) (foldl' adjust M.empty indices)
	where
	indices = [0..(getRowCount frame-1)]
	adjust store index =
		M.insertWith' (\n o -> mergePartials agg [n, o])
			(makeKey frame index) (processSingle agg frame index) store

