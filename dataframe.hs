module Dataframe
	where

import qualified Data.Map as M
import Data.Array

type Name = String

data Dataframe = Dataframe (M.Map Name Column, Int)
instance Show Dataframe where
	show (Dataframe (columns, rowCount)) =
		(show rowCount)++ " rows, "++(show $ M.size columns)++" columns"

data Column =
	IntC (Array Int Int)
	| StringC (Array Int String)
	| DoubleC (Array Int Double)

instance Show Column where
	show (IntC array) = show $ elems array
	show (StringC array) = show $ elems array
	show (DoubleC array) = show $ elems array

testFrame :: Dataframe
testFrame = makeFrame [
	("int", Left [1,2,3,4,5]),
	("string", Right $ Left ["A","B","C","B","A"]),
	("double", Right $ Right [2.0, 4.0, 6.0, 8.0, 10.0])
	]

columnLength :: Column -> Int
columnLength (IntC array) = length $ indices array
columnLength (StringC array) = length $ indices array
columnLength (DoubleC array) = length $ indices array

makeColumn :: Either [Int] (Either [String] [Double]) -> Column
makeColumn (Left ints) = IntC (listArray (0,length ints - 1) ints)
makeColumn (Right (Left strings)) =
	StringC (listArray (0,length strings - 1) strings)
makeColumn (Right (Right doubles)) =
	DoubleC (listArray (0,length doubles - 1) doubles)

makeFrame :: [(Name, Either [Int] (Either [String] [Double]))]
	-> Dataframe
makeFrame rawColumns =
	let columns = map (\(x,y)->(x,makeColumn y)) rawColumns in
	let rowCount = minimum $ (map columnLength) $ snd $ unzip columns in
	Dataframe (M.fromList columns, rowCount)

getColumn :: Dataframe -> Name -> Column
getColumn (Dataframe (columns, _)) columnName =
	let mColumn = M.lookup columnName columns in
	case mColumn of
		Nothing -> error ("Column "++columnName++" not found!")
		Just c -> c
