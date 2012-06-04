module NaiveBayes
	where

import qualified Data.Map as M

data NaiveBayes = ((Double, Double), [(String, ColumnBayes)])
data ColumnBayes = (Discretiser, M.Map Int (Double, Double))
data ColumnBayes = ([(Name, Discretiser)], Map String Double)
-- Discretiser for each column

-- Total length and y count
responseStats :: Matcher m => m -> (Double, Double)

-- For each column, x and x|y
columnStats :: Matcher m => m -> (DataColumn -> Discretiser)
	-> ColumnBayes

trainNaiveBayes :: Matcher m => m -> Dataframe -> NaiveBayes

-- Need multipass generation of discretisers
