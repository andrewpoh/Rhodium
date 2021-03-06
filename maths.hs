module Maths
	where

import Data.List

sum' :: Num a => [a] -> a
sum' = foldl' (+) 0

mean' :: (Fractional a, Num a) => [a] -> a
mean' xs = (sum' xs) / (fromIntegral . length) xs

logLoss :: Double -> Double -> Double
logLoss x y = ((x * log yb) + ((1.0-x) * log (1.0-yb)))* (-1.0)
	where
	yb = min 0.9999 (max 0.0001 y)
