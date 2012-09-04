module Rhodium.Models.DecisionTree
	where

import Data.Function
import Data.List
import Data.Maybe
import System.Random
import Rhodium.Data.DataColumn
import Rhodium.Data.Dataframe
import Rhodium.Data.DataframeParser
import Rhodium.Segment.Aggregators
import Rhodium.Segment.Matchers
import Rhodium.Segment.Segments
import Algorithms
import Maths

data DecisionTree a =
	DtSplit AnyMatcher (DecisionTree a) (DecisionTree a)
	| DtLeaf a
	deriving (Show)

runTree :: Dataframe -> DecisionTree a -> Int -> a
runTree _ (DtLeaf result) _ = result
runTree frame (DtSplit matcher trueBranch falseBranch) index =
	if matchOne matcher frame index
		then runTree frame trueBranch index
		else runTree frame falseBranch index

-- minsize, minstep, response, variables
data TreeConfig = TreeConfig Int Int Name [Name]
	deriving (Eq, Show)


