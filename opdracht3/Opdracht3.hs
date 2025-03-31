module Opdracht3 where
import Data.Bits
import Data.Char
import Data.List


{- 
Opdracht 1
Teststrings
-}
t1 :: String
t1 = "Hello world!"

t2 :: String
t2 = "Hello world! I am the world!"

t3 :: String
t3 = "Hello world! I am the world! I love Haskell"

testKey :: String
testKey = "verysecretkeybro"


{-
Opdracht 2
Basisfuncties
-}
String2Ints :: String -> [Int]
String2Ints = map ord

Ints2String :: [Int] -> String
Ints2String = map chr

pad :: Int -> [Int] -> [Int]
pad n xs = 
    let r = lenth xs `mod` n
    in if r == 0 then xs else xs ++ replicate (n - r) 0

hack :: Int -> [a] -> [[a]]
hack _ [] = []
hack n xs = take n xs : hack n (drop n xs)

type Block = [[Int]]
type Key = [Int]

-- Converteert een string naar blokken
string2blocks :: String -> [Block]
string2blocks = hack 4 . hack 4 . pad 16 . map fromEnum

-- Converteert blokken terug naar een string
blocks2string :: [Block] -> String
blocks2string = map toEnum . concat . concat


{-
Opdracht 3
Permutaties
-}
rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

rotblock :: Block -> Block
rotblock [r1, r2, r3, r4] =
    [ r1
    , rotateList 1  r2
    , rotateList 2  r3
    , rotateList 3  r4
    ]
rotblock _ = error "Block moet 4 rijen hebben"

invriotblock :: Block -> Block
invrotblock [r1, r2, r3, r4] =
    [ r1
    , rotateList (-1) r2
    , rotateList (-2) r3
    , rotateList (-3) r4
    ]
invrotblock _ = error "Block moet 4 rijen hebben"

rotblockbycolumn :: Block -> Block
rotblockbycolumn = transpose . rotblock . transpose

invrotblockbycolumn :: Block -> Block
invrotblockbycolumn = transpose . invrotblock . transpose

{-
Opdracht 4
Sleutelgeneratie en Vernam cypher
-}

rotatelist :: [a] -> Int -> [a]
rotateList n xs
    | null xs   = []
    | otherwise =
        let len = length xs
            n'  = (n `mod` len + len) `mod` len  -- Voorkomt negatieve indexen
        in drop n' xs ++ take n' xs

genroundkeys :: Key -> Int -> [Key]
genroundkeys key n = [rotateList i key | i <- [0 .. n-1]]

block2list :: Block -> [Int]
block2list = concat

blockxorkey :: Block -> Key -> Block
blockxorkey block key = hack 4 (xorLists (block2list block) (cycle key))


{-
Opdracht 5
Substitutie
-}
vigenere :: [Int] -> [Int] -> [Int]

invvigenere :: [Int] -> [Int] -> [Int]

vigenereblock :: Block -> Key -> Block

invvigenereblock :: Block -> Key -> Block


{-
Opdracht 6
Encryptiefunctie
-}
encryptround :: Block -> Key -> Block

decryptround :: Block -> Key -> Block

encryptrounds :: Key -> Block -> Block

decryptrounds :: Key -> Block -> Block

encrypt :: String -> String -> String

decrypt :: String -> String -> String