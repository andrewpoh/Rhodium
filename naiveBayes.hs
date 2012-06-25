{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
module NaiveBayes
	where

import Data.List
import qualified Data.Map as M
import Rhodium.Data.Dataframe
import Rhodium.Data.DataColumn
import Rhodium.Segment.Discretisers
import Rhodium.Segment.Matchers

data NaiveBayes = NaiveBayes (ResponseBayes, [(String, ColumnBayes)])
data ResponseBayes = ResponseBayes (Double, Double)
data ColumnBayes = forall d. (Discretiser d Int, Show d) =>
	ColumnBayes (d, M.Map Int (Double, Double))

instance Show NaiveBayes where
	show (NaiveBayes (r, l)) = show r ++ " - " ++ show l

instance Show ResponseBayes where
	show (ResponseBayes s) = show s

instance Show ColumnBayes where
	show (ColumnBayes (d, m)) = show d ++ " : " ++ show (M.toAscList m)

-- Discretiser for each column

-- Total length and y count
responseStats :: Matcher m => m -> Dataframe -> [Int] -> ResponseBayes
responseStats r f ixs =
	let totalLength = fromIntegral $ length ixs in
	let matchResults = matchMany r f ixs in
	let trueCount = (fromIntegral . length . filter id) matchResults in
	ResponseBayes (totalLength, trueCount)

-- For each column, x and x|y
columnStats :: (Matcher m, Discretiser d Int, Show d) =>
	m -> (Dataframe -> Name -> d) -> Dataframe -> [Int] -> Name
	-> ColumnBayes
columnStats r makeDiscretiser f ixs columnName =
	let discretiser = makeDiscretiser f columnName in
	let rowLevels = discretiseMany discretiser f ixs in
	let responses = matchMany r f ixs in
	let countsLookup =
		foldl' insertIntoMap M.empty (zip rowLevels responses) in
	ColumnBayes (discretiser, countsLookup)
	where
	addAdjust :: (Double,Double)->(Double,Double)->(Double,Double)
	addAdjust (a, b) (c, d) = (a+c, b+d)
	insertIntoMap :: M.Map Int (Double, Double) -> (Int, Bool) -> M.Map Int (Double, Double)
	insertIntoMap m (level, response) = 
		let rFlag = if response then 1.0 else 0.0 in
		M.insertWith' addAdjust level (1.0, rFlag) m
-- TODO: factor out foldl' insertIntoMap, this map construction is too common

testBayes :: Dataframe -> Name -> Name -> ColumnBayes
testBayes frame responseName columnName =
	let responseMatcher = IntSplit (responseName, 1) in
	let simpleIntDisc f n = IntDisc n in
	let allIndices = getIndices frame in
	columnStats responseMatcher simpleIntDisc frame allIndices columnName

simpleFrame :: Dataframe
simpleFrame = makeFrame [
	("response", Left [0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1]),
	("gender", Left [2,2,3,3,2,3,2,2,3,3,2,2,2,3,3,3,2,2,3,3]),
	("age", Left [4,5,5,4,4,5,5,5,4,5,5,4,5,5,5,4,5,4,5,5])
	]

testNaiveBayes :: Dataframe -> Name -> [Name] -> NaiveBayes
testNaiveBayes frame responseName xs =
	let responseMatcher = IntSplit (responseName, 1) in
	let simpleIntDisc f n = IntDisc n in
	let allIndices = getIndices frame in
	trainNaiveBayes responseMatcher simpleIntDisc frame allIndices xs

trainNaiveBayes :: (Matcher m, Discretiser d Int, Show d) =>
	m -> (Dataframe -> Name -> d) -> Dataframe -> [Int] -> [Name]
	-> NaiveBayes
trainNaiveBayes response makeDiscretiser frame ixs independentColumns =
	let responseBayes = responseStats response frame ixs in
	let makeColumnBayes = columnStats response makeDiscretiser frame ixs in
	let columnBayesList = map makeColumnBayes independentColumns in
	NaiveBayes (responseBayes, (zip independentColumns columnBayesList))

-- y|x = x|y * y / x
scoreNaiveBayes :: NaiveBayes -> Dataframe -> [Int] -> [Double]
scoreNaiveBayes (NaiveBayes (ResponseBayes (allFreq, yFreq), cs)) frame ixs =
	let prY = yFreq/allFreq in
	let cbs = map snd cs in
	let prXs = map (scoreColumnBayes frame ixs allFreq yFreq) cbs in
	foldl' (zipWith (*)) (replicate (getRowCount frame) prY) prXs

scoreColumnBayes :: Dataframe -> [Int] -> Double -> Double -> ColumnBayes -> [Double]
scoreColumnBayes frame ixs allFreq yFreq (ColumnBayes (d, m)) =
	let keys = discretiseMany d frame ixs in
	let levelValues = M.map calcPr m in
	map ((M.!) levelValues) keys
	where
	calcPr (levelFreq, levelYFreq) = levelYFreq/yFreq / levelFreq*allFreq

-- Need multipass generation of discretisers
