module DecisionTree
	where

import Segments

data DecisionTree a =
	DtSplit Matcher (DecisionTree a) (DecisionTree a)
	| DtLeaf a

runTree :: Dataframe -> Int -> DecisionTree a -> a
runTree _ _ (DtLeaf result) = result
runTree frame index (DtSplit matcher trueBranch falseBranch) =
	if matcher frame index
		then runTree frame index trueBranch
		else runTree frame index falseBranch