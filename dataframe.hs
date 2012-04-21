module Dataframe
	where

import qualified Data.List as L
import qualified Data.Map as M
import Data.Array
import Text.PrettyPrint as P
import DataCell
import DataColumn

type Name = String

data Dataframe = Dataframe (M.Map Name DataColumn, Int)
instance Show Dataframe where
	show df@(Dataframe (columns, rowCount)) =
		let summaryText = showSummary df in
		let columnDocs = ((map docColumn) . M.assocs) columns in
		let mergedColumns = ((map P.sep) . L.transpose) columnDocs in
		let columnText = (P.render . P.vcat) mergedColumns in
		summaryText ++ '\n':columnText

docColumn :: (Name, DataColumn) -> [P.Doc]
docColumn (n, (IntC array)) = (P.text n):(docArray P.int array)
docColumn (n, (DoubleC array)) = (P.text n):(docArray P.double array)
docColumn (n, (StringC array)) = (P.text n):(docArray P.text array)

docArray :: Ix ix => (d -> Doc) -> Array ix d -> [Doc]
docArray f = (map f) . elems

showSummary :: Dataframe -> String
showSummary (Dataframe (columns, rowCount)) =
	'(':(show rowCount)++ " rows, "++(show $ M.size columns)++" columns)"

testFrame :: Dataframe
testFrame = makeFrame [
	("int", Left [1,2,3,4,5]),
	("string", Right $ Left ["A","B","C","B","A"]),
	("double", Right $ Right [2.0, 4.0, 6.0, 8.0, 10.0])
	]


testCol1 = ("a", makeColumn (Left [1,2,3]))

getRowCount :: Dataframe -> Int
getRowCount (Dataframe (_, r)) = r

makeFrame :: [(Name, Either [Int] (Either [String] [Double]))]
	-> Dataframe
makeFrame rawColumns =
	let columns = map (\(x,y)->(x,makeColumn y)) rawColumns in
	let rowCount = minimum $ (map columnLength) $ snd $ unzip columns in
	Dataframe (M.fromList columns, rowCount)

makeDataframe :: Int -> [(Name, DataColumn)] -> Dataframe
makeDataframe rowCount columns =
	if any (\(_, c) -> columnLength c /= rowCount) columns
		then error "Column length mismatch!"
		else Dataframe (M.fromList columns, rowCount)

getColumn :: Dataframe -> Name -> DataColumn
getColumn (Dataframe (columns, _)) columnName =
	let mColumn = M.lookup columnName columns in
	case mColumn of
		Nothing -> error ("Column "++columnName++" not found!")
		Just c -> c
