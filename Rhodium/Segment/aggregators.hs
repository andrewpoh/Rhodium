{-# LANGUAGE TypeFamilies #-}
module Rhodium.Segment.Aggregators
	where

import qualified Data.Array.Unboxed as A
import Data.List
import qualified Data.Map as M
import Rhodium.Data.Dataframe
import Rhodium.Data.DataColumn

class Aggregator a where
	type AggPartial a :: *
	type AggResult a :: *
	processSingle :: a -> Dataframe -> Int -> AggPartial a
	processMany :: a -> Dataframe -> [Int] -> AggPartial a
	mergePartials :: a -> [AggPartial a] -> AggPartial a
	partialToFinal :: a -> AggPartial a -> AggResult a
	aggregateMany :: a -> Dataframe -> [Int] -> AggResult a
	aggregateMany a f ixs = partialToFinal a (processMany a f ixs)

data CountAgg = CountAgg
	deriving (Show,Eq)

instance Aggregator CountAgg where
	type AggPartial CountAgg = Int
	type AggResult CountAgg = Int
	processSingle _ _ _ = 1
	processMany _ _ = length
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

data MeanAgg = MeanAgg Name
	deriving (Show,Eq)

instance Aggregator MeanAgg where
	type AggPartial MeanAgg = (Double, Double)
	type AggResult MeanAgg = Double
	processSingle (MeanAgg columnName) frame index = (getDoubleColumn frame columnName A.! index, 1.0)
	processMany (MeanAgg columnName) frame indices =
		let column = getDoubleColumn frame columnName in
		(sum $ map ((A.!) column) indices, fromIntegral $ length indices)
	mergePartials _ partials = (sum $ map fst partials, sum $ map snd partials)
	partialToFinal _ (n,d) = n/d
