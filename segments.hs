module Segments
	where

import qualified Data.Array as A
import Data.List
import qualified Data.Map as M
import Dataframe
import DataColumn
import DataCell

class MatcherT m where
	matchOne :: m -> Dataframe -> Int -> Bool
	matchMany :: m -> Dataframe -> [Int] -> [Bool]
	matchMany m frame = map (matchOne m frame)
	matchAll :: m -> Dataframe -> [Bool]
	matchAll m frame = matchMany m frame (getIndices frame)

instance MatcherT IntSplit where
	matchOne (IntSplit (name, x)) frame index =
		let cell = getCell (getColumn frame name) index in
		fromIntCell cell < x
	matchMany (IntSplit (name, x)) frame indices =
		let array = fromIntC $ getColumn frame name in
		map (\i -> array A.! i < x) indices
	matchAll (IntSplit (name, x)) frame =
		let array = fromIntC $ getColumn frame name in
		map (<x) (A.elems array)

-- Simple <
newtype IntSplit = IntSplit (Name, Int)
	deriving (Eq, Show)

data Discretiser k =
	Discretiser (Dataframe -> Int -> k)

data Aggregator a =
	Aggregator (a, Dataframe -> Int -> a -> a)

aggregate :: Ord k=>Discretiser k -> Aggregator a -> Dataframe -> M.Map k a
aggregate (Discretiser makeKey) (Aggregator (seed, combine)) frame =
	foldl' adjust M.empty indices
	where
	indices = [0..(getRowCount frame-1)]
	adjust store index =
		M.insertWith' (\_ x -> combine frame index x)
			(makeKey frame index) seed store

countAgg :: Aggregator Int
countAgg = Aggregator (1, \_ _ x-> x+1)

simpleDisc :: String -> Discretiser String
simpleDisc columnName =
	Discretiser (\frame index -> showCell $ getCell (getColumn frame columnName) index)

intDisc :: String -> Discretiser Int
intDisc columnName = 
	Discretiser (\frame index -> fromIntCell $ getCell (getColumn frame columnName) index)

intSplits :: Dataframe -> Int -> Int -> String -> [IntSplit]
intSplits frame minSize minStep intColName =
	let histogram = M.assocs $ aggregate (intDisc intColName) countAgg frame in
	let totalFrequencies = (sum . map snd) histogram in
	let initialFreq = snd $ head histogram in
	let thist = tail histogram in
	let op = chooseSplit_ minSize minStep (getRowCount frame) intColName in
	if length histogram < 2 then []
		else snd $ foldl op ((initialFreq, initialFreq), []) thist

chooseSplit_ ::
	Int -> Int -> Int -> String
	-> ((Int, Int), [IntSplit]) -> (Int, Int)
	-> ((Int, Int), [IntSplit])
chooseSplit_ minSize minStep totalf columnName
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
