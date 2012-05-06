module Maths
	where

logLoss :: Double -> Double -> Double
logLoss x y = ((x * log yb) + ((1.0-x) * log (1.0-yb)))* (-1.0)
	where
	yb = min 0.9999 (max 0.0001 y)
