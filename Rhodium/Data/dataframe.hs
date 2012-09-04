module Rhodium.Data.Dataframe (
	Name,
	Dataframe, getRowCount, getColumnCount, getColumnNames, getIndices,
	toColumnList, makeDataframe,
	getColumn, getIntColumn, getDoubleColumn, getStringColumn)
	where

import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Vector as B
import qualified Data.Vector.Unboxed as V
-- Require the Boxes package to print dataframes neatly
import Text.PrettyPrint.Boxes as P
import Rhodium.Data.DataCell
import Rhodium.Data.DataColumn

type Name = T.Text

data Dataframe = Dataframe (M.Map Name DataColumn, Int)
instance Show Dataframe where
	show df@(Dataframe (columns, rowCount)) =
		let summaryBox = boxSummary df in
		let columnBoxes = (map boxColumn . M.assocs) columns in
		let mergedColumns = P.hsep 1 P.top columnBoxes in
		P.render (mergedColumns P.// summaryBox)

boxColumn :: (Name, DataColumn) -> P.Box
boxColumn (n, IntC array)
	= P.vcat P.right $ P.text (T.unpack n) : boxUVector (P.text . show) array 
boxColumn (n, DoubleC array)
	= P.vcat P.right $  P.text (T.unpack n) : boxUVector (P.text . show) array
boxColumn (n, StringC array)
	= P.vcat P.right $ P.text (T.unpack n) : boxBVector (P.text . T.unpack) array

boxUVector :: V.Unbox d => (d -> Box) -> V.Vector d -> [Box]
boxUVector f = map f . V.toList

boxBVector :: (d -> Box) -> B.Vector d -> [Box]
boxBVector f = map f . B.toList

boxSummary :: Dataframe -> P.Box
boxSummary frame =
	P.text ('(':show (getRowCount frame) ++ " rows, "
		++show (getColumnCount frame) ++ " columns)")

getRowCount :: Dataframe -> Int
getRowCount (Dataframe (_, r)) = r

getColumnCount :: Dataframe -> Int
getColumnCount (Dataframe (cs, _)) = M.size cs

getColumnNames :: Dataframe -> [Name]
getColumnNames (Dataframe (cs, _)) = M.keys cs

getIndices :: Dataframe -> [Int]
getIndices frame = [0..getRowCount frame - 1]

toColumnList :: Dataframe -> [(Name, DataColumn)]
toColumnList (Dataframe (cs, _)) = M.assocs cs

makeDataframe :: Int -> [(Name, DataColumn)] -> Dataframe
makeDataframe rowCount columns =
	if any (\(_, c) -> columnLength c /= rowCount) columns
		then error "Column length mismatch!"
		else Dataframe (M.fromList columns, rowCount)

getColumn :: Dataframe -> Name -> DataColumn
getColumn (Dataframe (columns, _)) columnName =
	let mColumn = M.lookup columnName columns in
	fromMaybe (error ("Column "++T.unpack columnName++" not found!")) mColumn

getIntColumn :: Dataframe -> Name -> V.Vector Int
getIntColumn f n = fromIntC $ getColumn f n

getDoubleColumn :: Dataframe ->  Name -> V.Vector Double
getDoubleColumn f n = fromDoubleC $ getColumn f n

getStringColumn :: Dataframe ->  Name -> B.Vector T.Text
getStringColumn f n = fromStringC $ getColumn f n
