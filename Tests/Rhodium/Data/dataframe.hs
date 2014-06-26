module Tests.Rhodium.Data.Dataframe
	where

makeColumn :: Either [Int] (Either [String] [Double]) -> DataColumn
makeColumn (Left ints) = IntC (V.fromList ints)
makeColumn (Right (Left strings)) =
	StringC (B.fromList (map T.pack strings))
makeColumn (Right (Right doubles)) =
	DoubleC (V.fromList doubles)


makeFrame :: [(String, Either [Int] (Either [String] [Double]))]
	-> Dataframe
makeFrame rawColumns =
	let columns = map (\(x,y)->(T.pack x,makeColumn y)) rawColumns in
	let rowCount = minimum $ map columnLength $ snd $ unzip columns in
	Dataframe (M.fromList columns, rowCount)

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
testCol2 = ("b", makeColumn (Right ["a","b","a"]))
