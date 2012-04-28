module DecisionTree
	where

import qualified Data.Array.Unboxed as A
import Data.List
import Data.Maybe
import Aggregators
import DataColumn
import Dataframe
import Matchers
import Segments

data DecisionTree a =
	DtSplit AnyMatcher (DecisionTree a) (DecisionTree a)
	| DtLeaf a
	deriving (Show)

runTree :: Dataframe -> Int -> DecisionTree a -> a
runTree _ _ (DtLeaf result) = result
runTree frame index (DtSplit matcher trueBranch falseBranch) =
	if matchOne matcher frame index
		then runTree frame index trueBranch
		else runTree frame index falseBranch

-- minsize, minstep, response, variables
data TreeConfig = TreeConfig Int Int Name [Name]
	deriving (Eq, Show)

growTree :: TreeConfig -> Dataframe -> [Int] -> DecisionTree Double
growTree tc@(TreeConfig _ _ r _) f ixs =
	let bm = bestMatcher tc f ixs in
	let toNode = makeNode tc f ixs in
	let leaf = DtLeaf (aggregateMany (MeanAgg r) f ixs) in
	maybe leaf toNode bm

makeNode :: TreeConfig -> Dataframe -> [Int] -> AnyMatcher
	-> DecisionTree Double
makeNode tc frame indices m =
	let (trues, falses) = partition (matchOne m frame) indices in
	let tLeft = growTree tc frame trues in
	let tRight = growTree tc frame falses in
	DtSplit m tLeft tRight

bestMatcher :: TreeConfig -> Dataframe -> [Int] -> Maybe AnyMatcher
bestMatcher (TreeConfig minSize minStep r ns) f ixs =
	let ms = makeMatchers minSize minStep f ixs ns in
	pickMatcher f r ixs ms

makeMatchers :: Int -> Int -> Dataframe -> [Int] -> [Name] -> [AnyMatcher]
makeMatchers minSize minStep f ixs ns =
	let cs = map (\n -> (n, getColumn f n)) ns in
	concatMap (makeMatcher minSize minStep f ixs) cs

makeMatcher :: Int -> Int -> Dataframe -> [Int]
	-> (Name, DataColumn) -> [AnyMatcher]
makeMatcher minSize minStep f ixs (n, c) = case columnType c of
	IntType -> map AnyMatcher (intSplits f minSize minStep n ixs)
	DblType -> map AnyMatcher (doubleSplits f minSize minStep n ixs)
	_ -> []

pickMatcher :: Matcher m => Dataframe -> Name -> [Int] -> [m] -> Maybe m
pickMatcher f r indices ms =
	let mScores = map (scoreMatcher f r indices) ms in
	let mAndScore = zip mScores ms in
	if length mAndScore > 0
		then Just $ snd $ maximumBy (\x y-> compare (fst x) (fst y)) mAndScore
		else Nothing

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
