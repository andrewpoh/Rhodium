{-# OPTIONS_GHC -funbox-strict-fields #-}

module Rhodium.Data.DataColumn
	where

import qualified Data.Text as T
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as B

import Rhodium.Data.DataCell

data DataColumn =
	IntC !(V.Vector Int)
	| StringC !(B.Vector T.Text)
	| DoubleC !(V.Vector Double)

instance Show DataColumn where
	show (IntC array) = "ints:" ++ show (V.toList array)
	show (StringC array) = "strings:" ++ show (B.toList array)
	show (DoubleC array) = "doubles:" ++ show (V.toList array)

data DataColumnType = IntType | DblType | StrType

columnType :: DataColumn -> DataColumnType
columnType (IntC _) = IntType
columnType (DoubleC _) = DblType
columnType (StringC _) = StrType

fromIntC :: DataColumn -> V.Vector Int
fromIntC (IntC array) = array
fromIntC _ = error "not int column"

fromStringC :: DataColumn -> B.Vector T.Text
fromStringC (StringC array) = array
fromStringC _ = error "not string column"

fromDoubleC :: DataColumn -> V.Vector Double
fromDoubleC (DoubleC array) = array
fromDoubleC _ = error "not double column"

mapColumn ::
	(V.Vector Int -> a)
	-> (V.Vector Double -> a)
	-> (B.Vector T.Text -> a)
	-> DataColumn -> a
mapColumn fInt _ _ (IntC array) = fInt array
mapColumn _ fDbl _ (DoubleC array) = fDbl array
mapColumn _ _ fStr (StringC array) = fStr array

getCell :: DataColumn -> Int -> DataCell
getCell (IntC array) ix = IntCell ((V.!) array ix)
getCell (DoubleC array) ix = DoubleCell ((V.!) array ix)
getCell (StringC array) ix = StringCell ((B.!) array ix)

columnLength :: DataColumn -> Int
columnLength (IntC array) = V.length array
columnLength (StringC array) = B.length array
columnLength (DoubleC array) = V.length array

cellsToColumn :: [DataCell] -> Maybe DataColumn
cellsToColumn cells
	| all isIntCell cells =
		Just $ IntC (V.fromList (map fromIntCell cells))
	| all isDoubleCell cells =
		Just $ DoubleC (V.fromList (map fromDoubleCell cells))
	| all isStringCell cells =
		Just $ StringC (B.fromList (map fromStringCell cells))
	| all (\x -> isDoubleCell x || isIntCell x) cells =
		Just $ DoubleC (V.fromList (map cellAsDouble cells))
	| otherwise = Nothing
