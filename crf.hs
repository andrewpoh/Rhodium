module ConditionalRandomField
	where

import Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import Text.ParserCombinators.Parsec
import Rhodium.Data.Dataframe
import Rhodium.Data.DataColumn
import Rhodium.Segment.Discretisers
import Rhodium.Segment.Matchers

-- parse sentences "filename" "Hello world! I'm Andrew."

type Sentence = [T.Text]

sentences :: Parser [Sentence]
sentences = sepEndBy1 sentence (oneOf ".?!")

sentence :: Parser Sentence
sentence = do
	optional separator
	words <- sepBy1 word separator
	return $! map T.pack words

word :: Parser String
word = many1 (letter <|> char '\'')

separator :: Parser ()
separator = skipMany1 (oneOf " ,;\"")
