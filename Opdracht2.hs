module Opdracht2 where
import Data.List

-- opdr1a
differentieer :: (Double -> Double) -> Double -> Double -> Double
differentieer f p x = (f (x+p) - f x ) / p

-- opdr1b
--integreer :: (Double->Double)->Double->Double->Double->Double
-- integreer f a b p =

-- opdr2
dubbelen :: Eq  a => [a] -> [a]
dubbelen s = nub (s \\ (nub s))