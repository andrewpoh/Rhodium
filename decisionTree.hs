--module DecisionTree
--	where
-- ghc -prof -auto-all -rtsopts DecisionTree.hs
-- +RTS -p

import qualified Data.Array.Unboxed as A
import Data.List
import Data.Maybe
import Random
import Rhodium.Data.DataColumn
import Rhodium.Data.Dataframe
import Rhodium.Data.DataframeParser
import Rhodium.Segment.Aggregators
import Rhodium.Segment.Matchers
import Rhodium.Segment.Segments
import Algorithms
import Maths

main :: IO ()
main = do
	forest <- makeForest
	scoreForest forest
--	tree <- makeTree
--	scoreTree tree

makeForest :: IO ([DecisionTree Double])
makeForest = do
	rawTable <- readFile "miniTrain.csv"
	let table = readTable rawTable
	let seed = mkStdGen 1337
	let names = tail (getColumnNames table)
	let namesArray = A.listArray (0, length names - 1) names
	let config = RandomTreeConfig 5 10 10 "Activity" namesArray
	let allIndices = getIndices table
	let (seed1, trees) = growRandomForest config 50 table allIndices seed
	return trees

scoreForest :: [DecisionTree Double] -> IO ()
scoreForest trees = do
	rawTable <- readFile "miniTest.csv"
	let table = readTable rawTable
	let predictions = map (\ix -> sum' (map (\t -> runTree table t ix) trees)/(fromIntegral (length trees))) (getIndices table)
	let response = A.elems (getDoubleColumn table "Activity")
	let treeScore = sum' (zipWith logLoss response predictions) / (fromIntegral (length response))
	print treeScore

scoreTree :: DecisionTree Double -> IO ()
scoreTree tree = do
	rawTable <- readFile "miniTest.csv"
	let table = readTable rawTable
	let predictions = map (runTree table tree) (getIndices table)
	let response = A.elems (getDoubleColumn table "Activity")
	let treeScore = sum' (zipWith logLoss response predictions) / (fromIntegral (length response))
	print treeScore

makeTree :: IO (DecisionTree Double)
makeTree = do
	rawTable <- readFile "miniTrain.csv"
	let table = readTable rawTable
	let seed = mkStdGen 1337
	let names = tail (getColumnNames table)
	let namesArray = A.listArray (0, length names - 1) names
	let config = RandomTreeConfig 5 9 9 "Activity" namesArray
	let allIndices = getIndices table
	let (seed1, tree) = growRandomTree config table allIndices seed
	return tree

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

-- mway minsize minstep response variables
data RandomTreeConfig = RandomTreeConfig Int Int Int Name (A.Array Int Name)
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

growRandomForest :: RandomGen g => RandomTreeConfig -> Int -> Dataframe
	-> [Int] -> g -> (g, [DecisionTree Double])
growRandomForest rtc 0 f ixs g = (g, [])
growRandomForest rtc n f ixs g =
	let (g1, tree) = growRandomTree rtc f ixs g in
	let (g2, trees) = growRandomForest rtc (n-1) f ixs g1 in
	(g2, tree:trees)

growRandomTree :: RandomGen g => RandomTreeConfig -> Dataframe -> [Int]
	-> g -> (g, DecisionTree Double)
growRandomTree rtc@(RandomTreeConfig _ _ _ r _) f ixs g =
	let (g1, bm) = randomBestMatcher rtc f ixs g in
	let toNode = makeRandomNode rtc f ixs g1 in
	let leaf = (g1, DtLeaf (aggregateMany (MeanAgg r) f ixs)) in
	maybe leaf toNode bm

makeRandomNode :: RandomGen g => RandomTreeConfig -> Dataframe -> [Int]
	-> g -> AnyMatcher -> (g, DecisionTree Double)
makeRandomNode rtc frame indices g m =
	let (trues, falses) = partition (matchOne m frame) indices in
	let (g1, tLeft) = growRandomTree rtc frame trues g in
	let (g2, tRight) = growRandomTree rtc frame falses g1 in
	(g2, DtSplit m tLeft tRight)

randomBestMatcher :: RandomGen g => RandomTreeConfig -> Dataframe -> [Int]
	-> g -> (g, Maybe AnyMatcher)
randomBestMatcher (RandomTreeConfig mWay mSize mStep r ns) f ixs g0 =
	let (g1, permutedNs) = permutation g0 ns in
	let sampledNs = take mWay permutedNs in
	let tc = TreeConfig mSize mStep r sampledNs in
	let (g2, rIxs) = sampleReplaceList ixs (length ixs) g1 in
	(g2, bestMatcher tc f rIxs)

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
	let (trues, falses) = partitionMany m frame indices in
	let sqDevFrame = sqDev frame r in
	let impurityAll = sqDevFrame indices in
	let impurityT = sqDevFrame trues in
	let impurityF = sqDevFrame falses in
	impurityAll - impurityT - impurityF

sqDev :: Dataframe -> Name -> [Int] -> Double
sqDev frame doubleName indices =
	let doubleColumn = getDoubleColumn frame doubleName in
	let ys = map ((A.!) doubleColumn) indices in
	let numerator = sum' ys in
	let denominator = (fromIntegral . length) ys in
	let average = numerator/denominator in
	let devSq = map (\x -> let d = average - x in d*d) ys in
	sum' devSq

-- This appears to have worse performance
sqDev2 :: Dataframe -> Name -> [Int] -> Double
sqDev2 frame doubleName indices =
	let doubleColumn = getDoubleColumn frame doubleName in
	let ys = map ((A.!) doubleColumn) indices in
	squaredDeviation' ys

squaredDeviation' :: [Double] -> Double
squaredDeviation' xs =
	let meanX = mean' xs in
	foldl' (\s x-> let d = meanX - x in d*d+s) 0.0 xs

squaredDeviation :: [Double] -> [Double] -> Double
squaredDeviation actual prediction =
	sum' $ zipWith (\x y-> let d = x - y in d * d) actual prediction

-- generate Matchers
-- calculate improvement
-- determine stopping condition (mostly pure, few samples, no attributes)
