{-# OPTIONS_GHC -XTypeFamilies #-}
module Segments
	where

import qualified Data.Array as A
import Data.List
import qualified Data.Map as M
import Dataframe
import DataColumn
import DataCell

class Matcher m where
	matchOne :: m -> Dataframe -> Int -> Bool
	matchMany :: m -> Dataframe -> [Int] -> [Bool]
	matchMany m frame = map (matchOne m frame)
	matchAll :: m -> Dataframe -> [Bool]
	matchAll m frame = matchMany m frame (getIndices frame)

instance Matcher IntSplit where
	matchOne (IntSplit (name, x)) frame index =
		let cell = getCell (getColumn frame name) index in
		fromIntCell cell < x
	matchMany (IntSplit (name, x)) frame indices =
		let array = getIntColumn frame name in
		map (\i -> array A.! i < x) indices
	matchAll (IntSplit (name, x)) frame =
		let array = getIntColumn frame name in
		map (<x) (A.elems array)

-- Simple <
newtype IntSplit = IntSplit (Name, Int)
	deriving (Eq, Show)

data Discretiser k =
	Discretiser (Dataframe -> Int -> k)

class Aggregator a where
	type AggPartial a :: *
	type AggResult a :: *
	processSingle :: a -> Dataframe -> Int -> AggPartial a
	processMany :: a -> Dataframe -> [Int] -> AggPartial a
	mergePartials :: a -> [AggPartial a] -> AggPartial a
	partialToFinal :: a -> AggPartial a -> AggResult a
	aggregateMany :: a -> Dataframe -> [Int] -> AggResult a
	aggregateMany a f ixs = (partialToFinal a (processMany a f ixs))

data CountAgg = CountAgg

instance Aggregator CountAgg where
	type AggPartial CountAgg = Int
	type AggResult CountAgg = Int
	processSingle _ _ _ = 1
	processMany _ _ indices = length indices
	mergePartials _ = sum
	partialToFinal _ = id

data CompoundAgg a b = CompoundAgg a b

instance (Aggregator a, Aggregator b) => Aggregator (CompoundAgg a b) where
	type AggPartial (CompoundAgg a b) = (AggPartial a, AggPartial b)
	type AggResult (CompoundAgg a b) = (AggResult a, AggResult b)
	processSingle (CompoundAgg a b) f i =
		(processSingle a f i, processSingle b f i)
	processMany (CompoundAgg a b) f ixs = 
		(processMany a f ixs, processMany b f ixs)
	mergePartials (CompoundAgg a b) partials =
		(mergePartials a (map fst partials),
			mergePartials b (map snd partials))
	partialToFinal (CompoundAgg a b) (pa, pb) =
		(partialToFinal a pa, partialToFinal b pb)

data MeanAgg = MeanAgg String

instance Aggregator MeanAgg where
	type AggPartial MeanAgg = (Double, Double)
	type AggResult MeanAgg = Double
	processSingle (MeanAgg columnName) frame index = (getDoubleColumn frame columnName A.! index, 1.0)
	processMany (MeanAgg columnName) frame indices =
		let column = getDoubleColumn frame columnName in
		(sum $ map ((A.!) column) indices, fromIntegral $ length indices)
	mergePartials _ partials = (sum $ map fst partials, sum $ map snd partials)
	partialToFinal _ (n,d) = n/d

aggregate :: (Ord k, Aggregator g) => Discretiser k -> g -> Dataframe -> M.Map k (AggResult g)
aggregate (Discretiser makeKey) agg frame =
	M.map (partialToFinal agg) (foldl' adjust M.empty indices)
	where
	indices = [0..(getRowCount frame-1)]
	adjust store index =
		M.insertWith' (\n o -> mergePartials agg [n, o])
			(makeKey frame index) (processSingle agg frame index) store

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
