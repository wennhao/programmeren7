module Opdracht2 where
import Data.List

-- opdr1a
differentieer :: (Double -> Double) -> Double -> Double -> Double
differentieer f p x = (f (x + p) - f x) / p

-- opdr1b
integreer :: (Double -> Double) -> Double -> Double -> Double -> Double
integreer f a b p = sum [ f x * p | x <- [a, a + p .. b - p] ]

-- opdr2
dubbelen :: Eq a => [a] -> [a]
dubbelen s = nub (s \\ nub s)

--opdr3
diceCounts :: [Int] -> [Int]
diceCounts dice = map length (group (sort dice))

isStraight :: [Int] -> Bool
isStraight dice =
  let s = sort dice
  in s == [1,2,3,4,5] || s == [2,3,4,5,6]

classifyRoll :: [Int] -> String
classifyRoll dice
  | sort counts == [5]       = "Poker"              -- alle 5 zijn gelijk
  | sort counts == [1,4]      = "Four of a Kind"     -- vier dezelfde en één anders
  | sort counts == [2,3]      = "Full House"         -- drie van een getal en twee van een ander
  | isStraight dice           = "Straight"           -- opeenvolgende getallen
  | sort counts == [1,1,3]    = "Three of a Kind"    -- drie dezelfde, de rest verschillend
  | sort counts == [1,2,2]    = "Two Pair"           -- twee paren
  | sort counts == [1,1,1,2]  = "One Pair"           -- één paar
  | otherwise                 = "Bust"               -- geen van bovenstaande
  where counts = diceCounts dice

-- allRolls :: [[Int]]
-- allRolls = [ [a, b, c, d, e] | a <- [1..6],
--                                b <- [1..6],
--                                c <- [1..6],
--                                d <- [1..6],
--                                e <- [1..6] ]

