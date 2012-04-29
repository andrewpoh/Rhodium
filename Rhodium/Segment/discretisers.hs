{-# LANGUAGE TypeFamilies #-}
module Rhodium.Segment.Discretisers
	where

import qualified Data.Array.Unboxed as A
import Rhodium.Data.DataCell
import Rhodium.Data.DataColumn
import Rhodium.Data.Dataframe

class Discretiser k where
	type DiscType k :: *
	discretiseSingle :: k -> Dataframe -> Int -> DiscType k
	discretiseMany :: k -> Dataframe -> [Int] -> [DiscType k]
	discretiseMany k f = map (discretiseSingle k f)

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
