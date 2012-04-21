module Segments
	where

import Data.List
import qualified Data.Map as M
import Dataframe
import DataColumn
import DataCell

data Matcher =
	Matcher (Dataframe -> Int -> Bool)

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
