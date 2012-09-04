{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Rhodium.Models.RandomForest
	where

import qualified Data.Text as T
import qualified Data.Vector as B
import qualified Data.Vector.Unboxed as V
import Data.Function
import Data.List
import Data.Maybe
import System.Random
import Rhodium.Models.DecisionTree
import Rhodium.Data.DataColumn
import Rhodium.Data.Dataframe
import Rhodium.Data.DataframeParser
import Rhodium.Segment.Aggregators
import Rhodium.Segment.Matchers
import Rhodium.Segment.Segments
import Algorithms
import Maths

-- Should swap boxed for unboxed
data UDouble3 = UDouble3 !Double !Double !Double
type BDouble3 = (Double, Double, Double)
type Forest a = [DecisionTree a]

predictForest :: Forest Double -> Dataframe -> [Double]
predictForest trees table = 
	let predictions = map (\ix -> sum' (map (\t -> runTree table t ix) trees)/(fromIntegral (length trees))) (getIndices table) in
	predictions

-- mway minsize minstep response variables
data RandomTreeConfig = RandomTreeConfig Int Int Int Name (B.Vector Name)
	deriving (Eq, Show)

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
	let ms = makeMatchers minSize minStep f ixs r ns in
	pickMatcher f r ixs ms

makeMatchers :: Int -> Int -> Dataframe -> [Int] -> Name -> [Name]
	-> [AnyMatcher]
makeMatchers minSize minStep f ixs r ns =
	let cs = map (\n -> (n, getColumn f n)) ns in
	let response = getDoubleColumn f r in
	concatMap (makeMatcher minSize minStep f response ixs) cs

makeMatcher :: Int -> Int -> Dataframe -> V.Vector Double -> [Int]
	-> (Name, DataColumn) -> [AnyMatcher]
makeMatcher minSize minStep f response ixs (n, c) = case columnType c of
	IntType -> map AnyMatcher (intSplits f minSize minStep n ixs)
	DblType ->
		let continuous = fromDoubleC c in
		let bestSplit = bestDoubleSplit minSize continuous response ixs in
		maybe []
			(\(split, _) -> [AnyMatcher (DoubleSplit (n,split))]) bestSplit
	_ -> []

bestDoubleSplit :: Int -> V.Vector Double -> V.Vector Double -> [Int]
	-> Maybe (Double, Double)
bestDoubleSplit minimumSize doubles response ix =
	let sorted = sortBy (compare `on` fst)
		(zip (map ((V.!) doubles) ix) (map ((V.!) response) ix)) in
	let justBefore = take minimumSize sorted in
	let justOnwards = drop minimumSize sorted in
	let lastX = fst $ last justBefore in
	let bN = fromIntegral minimumSize in
	let bY = sum $ map snd justBefore in
	let bYSquared = sum $ map (\(_,x)->x*x) justBefore in
	let before0 = (bN, bY, bYSquared) in
	let ys = map snd sorted in
	let allN = fromIntegral $ length sorted in
	let allY = sum' ys in
	let allYSquared = foldl' (\s y -> y*y+s) 0.0 ys in
	let possibleSplitCount = length sorted - (2*minimumSize) + 1 in
	let rest = take possibleSplitCount justOnwards in
	let after0 = (allN-bN, allY-bY, allYSquared-bYSquared) in
	let (_, _, _, result) = foldl' findDoubleSplit
		(lastX, before0, after0, Nothing) rest in
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
		let bMean = bY/bN in
		let beforeSquaredDev =
			bMean * bMean * bN + bYSquared - (2*bY*bMean) in
		let aMean = aY/aN in
		let afterSquaredDev =
			aMean * aMean * aN + aYSquared - (2*aY*aMean) in
		let scoreHere = beforeSquaredDev + afterSquaredDev in
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

