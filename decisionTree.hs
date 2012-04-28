module DecisionTree
	where

import qualified Data.Array.Unboxed as A
import Data.List
import DataColumn
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

{-
makeMatchers :: Dataframe -> [Int] -> [Name] -> AnyMatcher
makeMatchers f ixs ns =
	let cs = columnList f in
	let
-}

makeMatcher :: Dataframe -> [Int] -> (Name, DataColumn) -> [AnyMatcher]
makeMatcher f ixs (n, c) = case columnType c of
	IntType -> map AnyMatcher (intSplits f 1 1 n ixs)
	DblType -> map AnyMatcher (doubleSplits f 1 1 n ixs)
	_ -> []

pickMatcher :: Matcher m => Dataframe -> Name -> [Int] -> [m] -> m
pickMatcher f r indices ms =
	let mScores = map (scoreSplitter f r indices) ms in
	let mAndScore = zip mScores ms in
	snd $ maximumBy (\x y-> compare (fst x) (fst y)) mAndScore

-- larger is better
scoreMatcher :: Matcher m => Dataframe -> Name -> [Int] -> m -> Double
scoreMatcher frame r indices m =
	let (trues, falses) = partition (matchOne m frame) indices in
	let impurityAll = sqDev frame r indices in
	let impurityT = sqDev frame r trues in
	let impurityF = sqDev frame r falses in
	impurityAll - impurityT - impurityF

sqDev :: Dataframe -> Name -> [Int] -> Double
sqDev frame doubleName indices =
	let doubleColumn = getDoubleColumn frame doubleName in
	let ys = map ((A.!) doubleColumn) indices in
	let numerator = sum ys in
	let denominator = (fromIntegral . length) ys in
	let average = numerator/denominator in
	let devSq = map (\x -> let d = average - x in d*d) ys in
	sum devSq

squaredDeviation :: [Double] -> [Double] -> Double
squaredDeviation actual prediction =
	sum $ zipWith (\x y-> let d = x - y in d * d) actual prediction

-- generate Matchers
-- calculate improvement
-- determine stopping condition (mostly pure, few samples, no attributes)
