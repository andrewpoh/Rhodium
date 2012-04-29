module Rhodium.Data.DataframeParser
	where

import Data.Either
import Data.List
import Data.Maybe
import Text.ParserCombinators.Parsec
import Rhodium.Data.Dataframe
import Rhodium.Data.DataCell
import Rhodium.Data.DataColumn

readTable :: String -> Dataframe
readTable text =
	let raw = parse rawTable "" text in
	either (error . show) processTable raw

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
	leadingNumber <- signedNumber
	doubleTail <- optionMaybe (char '.' >> many1 digit)
	exponent <- optionMaybe (oneOf "eE" >> signedNumber)
	let isDouble = isJust doubleTail || isJust exponent
	let wholeNumber = leadingNumber
		++ maybe "" (\x -> '.':x) doubleTail
		++ maybe "" (\x -> 'e':x) exponent
	if not isDouble
		then return $ Left (read leadingNumber)
		else return $ Right (read wholeNumber)

signedNumber :: Parser String
signedNumber = do
	numberSign <- optionMaybe (oneOf "-+")
	numberBody <- many1 digit
	let x = if numberSign == Just '-' then '-':numberBody else numberBody
	return x

rowLine :: Parser [DataCell]
rowLine = commaSepLine1 dataCell

-- TODO: Strings

dataCell :: Parser DataCell
dataCell = integerOrDouble >>= (return . intDoubleToCell)

intDoubleToCell :: Either Int Double -> DataCell
intDoubleToCell (Left x) = IntCell x
intDoubleToCell (Right x) = DoubleCell x
