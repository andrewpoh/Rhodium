{-# OPTIONS_GHC -XTypeFamilies #-}
module Discretisers
	where

import qualified Data.Array.Unboxed as A
import DataCell
import DataColumn
import Dataframe

class Discretiser k where
	type DiscType k :: *
	discretiseSingle :: k -> Dataframe -> Int -> DiscType k
	discretiseMany :: k -> Dataframe -> [Int] -> [DiscType k]
	discretiseMany k f ixs = map (discretiseSingle k f) ixs

data ShowDisc = ShowDisc Name
instance Discretiser ShowDisc where
	type DiscType ShowDisc = String
	discretiseSingle (ShowDisc n) f i =
		let cell = getCell (getColumn f n) i in
		showCell cell

data IntDisc = IntDisc Name
instance Discretiser IntDisc where
	type DiscType IntDisc = Int
	discretiseSingle (IntDisc n) f i =
		let intColumn = getIntColumn f n in
		intColumn A.! i
