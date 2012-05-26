{-# LANGUAGE TypeFamilies #-}
module Rhodium.Segment.Discretisers
	where

import qualified Data.Array.Unboxed as A
import qualified Data.Map as M
import Rhodium.Data.DataCell
import Rhodium.Data.DataColumn
import Rhodium.Data.Dataframe

class Discretiser k where
	type DiscType k :: *
	discretiseSingle :: k -> Dataframe -> Int -> DiscType k
	discretiseMany :: k -> Dataframe -> [Int] -> [DiscType k]
	discretiseMany k f = map (discretiseSingle k f)

data IntGrouping = IntGrouping Name (M.Map Int Int) Int
instance Discretiser IntGrouping where
	type DiscType IntGrouping = Int
	discretiseSingle (IntGrouping n mappings defaultLevel) f ix =
		let datum = getIntColumn f n A.! ix in
		M.findWithDefault defaultLevel datum mappings
	discretiseMany (IntGrouping n mappings defaultLevel) f ixs =
		let columnArray = getIntColumn f n in
		map (\ix ->
			M.findWithDefault defaultLevel (columnArray A.! ix) mappings)
			ixs

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
