module Matchers
	where

import qualified Data.Array.Unboxed as A
import DataCell
import DataColumn
import Dataframe

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

newtype DoubleSplit = DoubleSplit (Name, Double)
	deriving (Eq, Show)
