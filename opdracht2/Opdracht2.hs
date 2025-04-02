module Opdracht2 where
import Data.List

-- opdr1a
-- differentieer (\x -> 2*x + 3) 0.0001 5   
differentieer :: (Double -> Double) -> Double -> Double -> Double
differentieer f p x = (f (x + p) - f x) / p

-- opdr1b
-- integreer (\x -> 2*x + 3) 0 5 0.0001
-- linker riemann som
integreer :: (Double -> Double) -> Double -> Double -> Double -> Double
integreer f a b p = sum [ f x * p | x <- [a, a + p .. b - p] ]

-- opdr2
-- de nub functie verwijderd dubbele elementen uit een lijst
-- de \\ operator verwijdert de elementen van de tweede lijst uit de eerste
dubbelen :: Eq a => [a] -> [a]
dubbelen s = nub (s \\ nub s)


{-
Opdracht 3 Poker
bepaal de kans van elk hand van alle mogelijke worpen
-}
-- telt het aantal keren dat elk getal voorkomt in de worp
diceCounts :: [Int] -> [Int]
diceCounts dice = map length (group (sort dice))

-- functie om de worp te classificeren
-- Poker, Four of a Kind, Full House, Three of a Kind, Two Pair, One Pair, Straight, Bust
isPoker :: [Int] -> Bool
isPoker dice = diceCounts dice == [5]

isFourOfAKind :: [Int] -> Bool
isFourOfAKind dice = diceCounts dice == [4,1] || diceCounts dice == [1,4]

isFullHouse :: [Int] -> Bool
isFullHouse dice = diceCounts dice == [3,2] || diceCounts dice == [2,3]

isThreeOfAKind :: [Int] -> Bool
isThreeOfAKind dice = diceCounts dice == [3,1,1] || diceCounts dice == [1,3,1] || diceCounts dice == [1,1,3] && not (isFullHouse dice)

isTwoPair :: [Int] -> Bool
isTwoPair dice = diceCounts dice == [2,2,1] || diceCounts dice == [1,2,2]

isOnePair :: [Int] -> Bool
isOnePair dice = diceCounts dice == [2,1,1,1] || diceCounts dice == [1,2,1,1] || diceCounts dice == [1,1,2,1] || diceCounts dice == [1,1,1,2]

isStraight :: [Int] -> Bool
isStraight dice =
  let s = sort dice
  in s == [1,2,3,4,5] || s == [2,3,4,5,6]

-- functie om combinaties van getallen met hands te vergelijken
determinehand :: [Int] -> String
determinehand dice
    | isPoker dice = "Poker"
    | isFourOfAKind dice = "Four of a Kind"
    | isFullHouse dice = "Full House"
    | isThreeOfAKind dice = "Three of a Kind"
    | isTwoPair dice = "Two Pair"
    | isOnePair dice = "One Pair"
    | isStraight dice = "Straight"
    | otherwise = "Bust"

-- functie om alle mogelijke worpen te genereren
allRolls :: [[Int]]
allRolls = [ [dice1, dice2, dice3, dice4, dice5] | dice1 <- [1..6],
    dice2 <- [1..6],
    dice3 <- [1..6],
    dice4 <- [1..6],
    dice5 <- [1..6]]

-- functie om de kansen van elke hand te berekenen
calculatechances :: [(String, Double)]
calculatechances = 
    let totalRolls = fromIntegral (length allRolls)
        hands = map determinehand allRolls
        uniqueHands = ["Poker", "Four of a Kind", "Full House", "Three of a Kind", "Two Pair", "One Pair", "Straight", "Bust"]
        counts = [(h, fromIntegral (length (filter (==h) hands)) / totalRolls * 100) | h <- uniqueHands]
    in counts


{- 

oud
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

-}