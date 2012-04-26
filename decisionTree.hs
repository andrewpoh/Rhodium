module DecisionTree
	where

import Dataframe
import Matchers
import Segments

data DecisionTree a =
	DtSplit AnyMatcher (DecisionTree a) (DecisionTree a)
	| DtLeaf a

runTree :: Dataframe -> Int -> DecisionTree a -> a
runTree _ _ (DtLeaf result) = result
runTree frame index (DtSplit matcher trueBranch falseBranch) =
	if matchOne matcher frame index
		then runTree frame index trueBranch
		else runTree frame index falseBranch

scoreSplitter :: Matcher m => Dataframe -> [Int] -> m -> Double
scoreSplitter frame indices m =
	let (trues, falses) = partition (matchOne m frame) indices in
	??

squaredDeviation :: [Double] -> [Double] -> Double
squaredDeviation actual prediction =
	sum $ zipWith (\x y-> let d = x - y in d * d) actual prediction

-- generate Matchers
-- calculate improvement
-- determine stopping condition (mostly pure, few samples, no attributes)
-- Use (weighted) squared deviation for node impurity
