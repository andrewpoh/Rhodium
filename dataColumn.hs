module DataColumn
	where

import qualified Data.Array.Unboxed as A

import DataCell

data DataColumn =
	IntC (A.UArray Int Int)
	| StringC (A.Array Int String)
	| DoubleC (A.UArray Int Double)

instance Show DataColumn where
	show (IntC array) = "ints:" ++ (show $ A.elems array)
	show (StringC array) = "strings:" ++ (show $ A.elems array)
	show (DoubleC array) = "doubles:" ++ (show $ A.elems array)

fromIntC :: DataColumn -> A.UArray Int Int
fromIntC (IntC array) = array
fromIntC _ = error "not int column"

fromStringC :: DataColumn -> A.Array Int String
fromStringC (StringC array) = array
fromStringC _ = error "not string column"

fromDoubleC :: DataColumn -> A.UArray Int Double
fromDoubleC (DoubleC array) = array
fromDoubleC _ = error "not double column"

getCell :: DataColumn -> Int -> DataCell
getCell (IntC array) ix = IntCell ((A.!) array ix)
getCell (DoubleC array) ix = DoubleCell ((A.!) array ix)
getCell (StringC array) ix = StringCell ((A.!) array ix)

columnLength :: DataColumn -> Int
columnLength (IntC array) = length $ A.indices array
columnLength (StringC array) = length $ A.indices array
columnLength (DoubleC array) = length $ A.indices array

cellsToColumn :: [DataCell] -> Maybe DataColumn
cellsToColumn cells
	| all isIntCell cells =
		Just $ IntC (A.listArray bounds (map fromIntCell cells))
	| all isDoubleCell cells =
		Just $ DoubleC (A.listArray bounds (map fromDoubleCell cells))
	| all isStringCell cells =
		Just $ StringC (A.listArray bounds (map fromStringCell cells))
	| otherwise = Nothing
	where
	bounds = (0,length cells - 1)

makeColumn :: Either [Int] (Either [String] [Double]) -> DataColumn
makeColumn (Left ints) = IntC (A.listArray (0,length ints - 1) ints)
makeColumn (Right (Left strings)) =
	StringC (A.listArray (0,length strings - 1) strings)
makeColumn (Right (Right doubles)) =
	DoubleC (A.listArray (0,length doubles - 1) doubles)
