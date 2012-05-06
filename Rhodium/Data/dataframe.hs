module Rhodium.Data.Dataframe
	where

import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import Data.Array.Unboxed
-- Require the Boxes package to print dataframes neatly
import Text.PrettyPrint.Boxes as P
import Rhodium.Data.DataCell
import Rhodium.Data.DataColumn

type Name = String

data Dataframe = Dataframe (M.Map Name DataColumn, Int)
instance Show Dataframe where
	show df@(Dataframe (columns, rowCount)) =
		let summaryBox = boxSummary df in
		let columnBoxes = (map boxColumn . M.assocs) columns in
		let mergedColumns = P.hsep 1 P.top columnBoxes in
		P.render (mergedColumns P.// summaryBox)

boxColumn :: (Name, DataColumn) -> P.Box
boxColumn (n, IntC array)
	= P.vcat P.right $ P.text n : boxArray (P.text . show) array 
boxColumn (n, DoubleC array)
	= P.vcat P.right $  P.text n : boxArray (P.text . show) array
boxColumn (n, StringC array)
	= P.vcat P.right $ P.text n : boxArray P.text array

boxArray :: (IArray a d, Ix ix) => (d -> Box) -> a ix d -> [Box]
boxArray f = map f . elems

boxSummary :: Dataframe -> P.Box
boxSummary frame =
	P.text ('(':show (getRowCount frame) ++ " rows, "
		++show (getColumnCount frame) ++ " columns)")

testFrame :: Dataframe
testFrame = makeFrame [
	("int", Left [1,2,3,4,5]),
	("string", Right $ Left ["A","B","C","B","A"]),
	("double", Right $ Right [2.0, 4.0, 6.0, 8.0, 10.0])
	]

bigFrame :: Dataframe
bigFrame = makeFrame [
	("r", Right $ Right [2.0, 4.0, 6.0, 8.0, 10.0, 3.0, 1.0, 9.0, 5.0, 7.0]),
	("i1", Left [1,2,3,4,5,2,3,3,4,5]),
	("i2", Left [3,2,4,4,3,2,2,3,4,2]),
	("d1", Right $ Right [3.5, 2.5, 4.0, 6.7, 5.2, 6.1, 4.5, 3.2, 4.2, 5.5])
	]

testCol1 = ("a", makeColumn (Left [1,2,3]))

getRowCount :: Dataframe -> Int
getRowCount (Dataframe (_, r)) = r

getColumnCount :: Dataframe -> Int
getColumnCount (Dataframe (cs, _)) = M.size cs

getColumnNames :: Dataframe -> [Name]
getColumnNames (Dataframe (cs, _)) = M.keys cs

getIndices :: Dataframe -> [Int]
getIndices frame = [0..getRowCount frame - 1]

columnList :: Dataframe -> [(Name, DataColumn)]
columnList (Dataframe (cs, _)) = M.assocs cs

makeFrame :: [(Name, Either [Int] (Either [String] [Double]))]
	-> Dataframe
makeFrame rawColumns =
	let columns = map (\(x,y)->(x,makeColumn y)) rawColumns in
	let rowCount = minimum $ map columnLength $ snd $ unzip columns in
	Dataframe (M.fromList columns, rowCount)

makeDataframe :: Int -> [(Name, DataColumn)] -> Dataframe
makeDataframe rowCount columns =
	if any (\(_, c) -> columnLength c /= rowCount) columns
		then error "Column length mismatch!"
		else Dataframe (M.fromList columns, rowCount)

getColumn :: Dataframe -> Name -> DataColumn
getColumn (Dataframe (columns, _)) columnName =
	let mColumn = M.lookup columnName columns in
	fromMaybe (error ("Column "++columnName++" not found!")) mColumn

getIntColumn :: Dataframe -> Name -> UArray Int Int
getIntColumn f n = fromIntC $ getColumn f n

getDoubleColumn :: Dataframe ->  Name -> UArray Int Double
getDoubleColumn f n = fromDoubleC $ getColumn f n

getStringColumn :: Dataframe ->  Name -> Array Int String
getStringColumn f n = fromStringC $ getColumn f n
