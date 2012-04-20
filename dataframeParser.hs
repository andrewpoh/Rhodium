module Dataframe.Parser
	where

import Data.List
import Data.Maybe
import Text.ParserCombinators.Parsec
import Dataframe
import DataCell

-- readTable :: String -> Dataframe

egTable :: String
egTable = "a,b\n-12,0.34\n3, 1.5"

processTable :: ([String],[[DataCell]]) -> Dataframe
processTable (h, rs) =
	let rowCount = length rs in
	let asColumns = transpose rs in
	let rawColumnPairs =
		zipWith (\x y -> (x, cellsToColumn y)) h asColumns in
	let uniformColumnPairs =
		filter (\(x,y) -> isJust y) rawColumnPairs in
	let columnPairs =
		map (\(x,y) -> (x, fromJust y)) uniformColumnPairs in
	makeDataframe rowCount columnPairs

rawTable :: Parser ([String],[[DataCell]])
rawTable = do
	h <- headerLine
	rs <- many1 rowLine
	return (h, rs)

headerLine :: Parser [String]
headerLine = commaSepLine1 headerName

headerName :: Parser String
headerName = many1 alphaNum

comma :: Parser Char
comma = char ','

commaSepLine1 :: Parser a -> Parser [a]
commaSepLine1 entryParser = do
	entries <- sepEndBy1 entryParser (comma >> many space)
	optional (char '\n')
	return entries

integerOrDouble :: Parser (Either Int Double)
integerOrDouble = do
	numberSign <- optionMaybe (oneOf "-+")
	numberLead <- many1 digit
	doubleTail <- optionMaybe (char '.' >> many1 digit)
	let performNegate = numberSign == Just '-'
	let int1 = read numberLead
	let int2 = if performNegate then -1 * int1 else int1
	let double1 = numberLead++'.':(fromJust doubleTail)
	let double2 = read double1
	let double3 = if performNegate then -1.0 * double2 else double2
	if isNothing doubleTail
		then return $ Left int2
		else return $ Right double3

rowLine :: Parser [DataCell]
rowLine = commaSepLine1 dataCell

-- TODO: Strings

dataCell :: Parser DataCell
dataCell = integerOrDouble >>= (return . intDoubleToCell)

intDoubleToCell :: Either Int Double -> DataCell
intDoubleToCell (Left x) = IntCell x
intDoubleToCell (Right x) = DoubleCell x
