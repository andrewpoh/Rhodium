module NaiveBayes
	where

import qualified Data.Map as M
import Dataframe

data NaiveBayes = ((Double, Double), [(String, ColumnBayes)])
data ResponseBayes = ResponseBayes (Double, Double)
data ColumnBayes = ColumnBayes (Discretiser, M.Map Int (Double, Double))
-- Discretiser for each column

-- Total length and y count
responseStats :: Matcher m => m -> Dataframe -> [Int] -> ResponseBayes
responseStats r f ixs =
	let totalLength = fromIntegral $ length ixs in
	let matchResults = matchMany r f ixs in
	let trueCount = (fromIntegral . length . filter id) matchResults in
	ResponseBayes (totalLength, trueCount)

-- For each column, x and x|y
columnStats :: (Matcher m, Discretiser d Int) =>
	m -> (DataColumn -> d) -> Dataframe -> [Int] -> ColumnBayes
columnStats r makeDiscretiser f =
	let discretiser = makeDiscretiser c in
	let rowLevels = discretiseMany discretiser f ixs in
	let responses = matchMany r f ixs in
	let countsLookup =
		foldl' insertIntoMap M.empty (zip rowLevels responses) in
	ColumnBayes (discretiser, countsLookup)
	where
	addDdjust (a, b) (c, d) = (a+c, b+d)
	insertIntoMap (level, response) = 
		let rFlag = if response then 1.0 else 0.0 in
		M.insertWith' addAdjust level (1.0, rFlag)

-- TODO: factor out insertIntoMap, this map construction is too common

trainNaiveBayes :: Matcher m => m -> Dataframe -> NaiveBayes

-- Need multipass generation of discretisers
