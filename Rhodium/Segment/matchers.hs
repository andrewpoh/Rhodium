{-# LANGUAGE ExistentialQuantification #-}
module Rhodium.Segment.Matchers
	where

import qualified Data.Array.Unboxed as A
import Data.List
import Rhodium.Data.DataCell
import Rhodium.Data.DataColumn
import Rhodium.Data.Dataframe

class Matcher m where
	matchOne :: m -> Dataframe -> Int -> Bool
	matchMany :: m -> Dataframe -> [Int] -> [Bool]
	matchMany m frame = map (matchOne m frame)
	matchAll :: m -> Dataframe -> [Bool]
	matchAll m frame = matchMany m frame (getIndices frame)
	partitionMany :: m -> Dataframe -> [Int] -> ([Int], [Int])
	partitionMany m frame = partition (matchOne m frame)

data AnyMatcher = forall m. (Matcher m, Show m) => AnyMatcher m

instance Show AnyMatcher where
	show (AnyMatcher m) = show m

instance Matcher AnyMatcher where
	matchOne (AnyMatcher m) = matchOne m
	matchMany (AnyMatcher m) = matchMany m
	matchAll (AnyMatcher m) = matchAll m
	partitionMany (AnyMatcher m) = partitionMany m

-- Simple <
newtype IntSplit = IntSplit (Name, Int)
	deriving (Eq, Show)

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
	partitionMany (IntSplit (name, x)) frame indices =
		let array = getIntColumn frame name in
		partition (\i -> array A.! i < x) indices

newtype DoubleSplit = DoubleSplit (Name, Double)
	deriving (Eq, Show)

instance Matcher DoubleSplit where
	matchOne (DoubleSplit (name, x)) frame index =
		let cell = getCell (getColumn frame name) index in
		fromDoubleCell cell < x
	matchMany (DoubleSplit (name, x)) frame indices =
		let array = getDoubleColumn frame name in
		map (\i -> array A.! i < x) indices
	matchAll (DoubleSplit (name, x)) frame =
		let array = getDoubleColumn frame name in
		map (<x) (A.elems array)
	partitionMany (DoubleSplit (name, x)) frame indices =
		let array = getDoubleColumn frame name in
		partition (\i -> array A.! i < x) indices
