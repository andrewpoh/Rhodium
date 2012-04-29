module Rhodium.Data.DataCell
	where

data DataCell =
	IntCell Int
	| StringCell String
	| DoubleCell Double
	deriving (Eq,Show)

showCell (IntCell x) = show x
showCell (DoubleCell x) = show x
showCell (StringCell x) = show x

isIntCell :: DataCell -> Bool
isIntCell (IntCell _) = True
isIntCell _ = False

isDoubleCell :: DataCell -> Bool
isDoubleCell (DoubleCell _) = True
isDoubleCell _ = False

isStringCell :: DataCell -> Bool
isStringCell (StringCell _) = True
isStringCell _ = False

fromIntCell :: DataCell -> Int
fromIntCell (IntCell x) = x
fromIntCell _ = error "Not an IntCell"

fromDoubleCell :: DataCell -> Double
fromDoubleCell (DoubleCell x) = x
fromDoubleCell _ = error "Not a DoubleCell"

cellAsDouble :: DataCell -> Double
cellAsDouble (DoubleCell x) = x
cellAsDouble (IntCell x) = fromIntegral x
cellAsDouble _ = error "Not a DoubleCell"

fromStringCell :: DataCell -> String
fromStringCell (StringCell x) = x
fromStringCell _ = error "Not a StringCell"
