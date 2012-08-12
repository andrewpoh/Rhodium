--module DecisionTree
--	where
-- ghc -prof -auto-all -rtsopts DecisionTree.hs
-- +RTS -p

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

import qualified Data.Text as T
import qualified Data.Vector as B
import qualified Data.Vector.Unboxed as V
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

main :: IO ()
main = do
--	doSquaredDeviation
	doForest

-- Should swap boxed for unboxed
data UDouble3 = UDouble3 !Double !Double !Double
type BDouble3 = (Double, Double, Double)

doSquaredDeviation :: IO ()
doSquaredDeviation = do
	let f = squaredDeviation
	let input = V.fromList [0..100000]
	let makeIndices seed = (take 20000) (randomRs (0, V.length input - 1) (mkStdGen seed))
	let doAcc acc i = f (map ((V.!) input) (makeIndices i))+acc
	let result0 = foldl' doAcc 0.0 [0..99]
	let result1 = foldl' doAcc 0.0 [0..99]
	let result2 = foldl' doAcc 0.0 [0..99]
	let result3 = foldl' doAcc 0.0 [0..99]
	let result4 = foldl' doAcc 0.0 [0..99]
	let result5 = foldl' doAcc 0.0 [0..99]
	let result6 = foldl' doAcc 0.0 [0..99]
	let result7 = foldl' doAcc 0.0 [0..99]
	let result8 = foldl' doAcc 0.0 [0..99]
	let result9 = foldl' doAcc 0.0 [0..99]
	print $ sum' [result0, result1, result2, result3, result4, result5, result6, result7, result8, result9]
	
doForest :: IO ()
doForest = do
	forest <- makeForest
	scoreForest forest

doTree :: IO ()
doTree = do
	tree <- makeTree
	scoreTree tree

makeForest :: IO ([DecisionTree Double])
makeForest = do
	rawTable <- readFile "miniTrain.csv"
	let table = readTable rawTable
	let seed = mkStdGen 1337
	let names = tail (getColumnNames table)
	let namesArray = B.fromList names
	let config = RandomTreeConfig 5 10 10 (T.pack "Activity") namesArray
	let allIndices = getIndices table
	let (seed1, trees) = growRandomForest config 50 table allIndices seed
	return trees

scoreForest :: [DecisionTree Double] -> IO ()
scoreForest trees = do
	rawTable <- readFile "miniTest.csv"
	let table = readTable rawTable
	let predictions = map (\ix -> sum' (map (\t -> runTree table t ix) trees)/(fromIntegral (length trees))) (getIndices table)
	let response = V.toList (getDoubleColumn table (T.pack "Activity"))
	let treeScore = sum' (zipWith logLoss response predictions) / (fromIntegral (length response))
	print treeScore

scoreTree :: DecisionTree Double -> IO ()
scoreTree tree = do
	rawTable <- readFile "miniTest.csv"
	let table = readTable rawTable
	let predictions = map (runTree table tree) (getIndices table)
	let response = V.toList (getDoubleColumn table (T.pack "Activity"))
	let treeScore = sum' (zipWith logLoss response predictions) / (fromIntegral (length response))
	print treeScore

makeTree :: IO (DecisionTree Double)
makeTree = do
	rawTable <- readFile "miniTrain.csv"
	let table = readTable rawTable
	let seed = mkStdGen 1337
	let names = tail (getColumnNames table)
	let namesArray = B.fromList names
	let config = RandomTreeConfig 5 9 9 (T.pack "Activity") namesArray
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
data RandomTreeConfig = RandomTreeConfig Int Int Int Name (B.Vector Name)
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

bestDoubleSplit :: V.Vector Double -> V.Vector Double
	-> Maybe (Double, Double)
bestDoubleSplit doubles response =
	let sorted = sortBy (compare `on` fst)
		(zip (V.toList doubles) (V.toList response)) in
	let duplicateHead = fst $ head sorted in
	let before0 = (0.0, 0.0, 0.0) in
	let allN = fromIntegral $ V.length response in
	let allY = V.sum response in
	let allYSquared = V.foldl' (\s y -> y*y+s) 0.0 response in
	let after0 = (allN, allY, allYSquared) in
	let (_, _, _, result) = foldl' findDoubleSplit
		(duplicateHead, before0, after0, Nothing) sorted in
	result

-- replace with VDouble3
findDoubleSplit :: (Double, BDouble3, BDouble3, Maybe (Double, Double))
	-> (Double, Double)
	-> (Double, BDouble3, BDouble3, Maybe (Double, Double))
findDoubleSplit (lastX, (bN, bY, bYSquared), (aN, aY, aYSquared), best)
	(x, y) =
	let ySquared = y*y in
	let bN1 = bN+1.0 in
	let bY1 = bY+y in
	let bYSquared1 = bYSquared+ySquared in
	let aN1 = aN-1.0 in
	let aY1 = aY - y in
	let aYSquared1 = aYSquared - ySquared in
	let before1 = (bN1, bY1, bYSquared1) in
	let after1 = (aN1, aY1, aYSquared1) in
	let moveOn = (x, before1, after1, best) in
	if x <= lastX then
		moveOn
	else
		let splitHere = (x+lastX)/2.0 in
		let bMean1 = bY1/bN1 in
		let beforeSquaredDev1 =
			bMean1 * bMean1 * bN1 + bYSquared1 - (2*bY1*bMean1) in
		let aMean1 = aY1/aN1 in
		let afterSquaredDev1 =
			aMean1 * aMean1 * aN1 + aYSquared1 - (2*aY1*aMean1) in
		let scoreHere = beforeSquaredDev1 + afterSquaredDev1 in
		let bestHere = Just (splitHere, scoreHere) in
		let best1 = maybe bestHere
			(\(bestSplit, bestScore) ->
				if scoreHere >= bestScore then best else bestHere) best in
		(x, before1, after1, best1)
		
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
	let ys = map ((V.!) doubleColumn) indices in
	squaredDeviation ys

-- mean(y)^2 * length + sum(y^2) -2* sum(y)^2/length
squaredDeviation' :: [Double] -> Double
squaredDeviation' xs =
	let (UDouble3 nX sumX sumX2) =
		foldl' squaredDeviationPass (UDouble3 0.0 0.0 0.0) xs in
	let !meanX = sumX/nX in
	let !r = meanX * meanX * nX + sumX2 - (2*sumX*sumX/nX) in
	r

squaredDeviationPass :: UDouble3 -> Double -> UDouble3
squaredDeviationPass (UDouble3 nX sumX sumX2) x =
	UDouble3 (nX+1) (sumX+x) (x*x+sumX2)

squaredDeviation :: [Double] -> Double
squaredDeviation xs =
	let !meanX = mean' xs in
	foldl' (deviationPlus' meanX) 0.0 xs

deviationPlus' :: Double -> Double -> Double -> Double
deviationPlus' m acc x =
	let !d = m - x in
	let !d2 = d * d in
	let !r = d2+acc in
	r

squaredDeviationSimple :: [Double] -> [Double] -> Double
squaredDeviationSimple actual prediction =
	sum' $ zipWith (\x y-> let !d = x - y in d * d) actual prediction
