module Opdracht3 where
import Data.Bits
import Data.Char
import Data.List

import Debug.Trace (trace)


{- 
Opdracht 1
Teststrings
-}
t1 :: String
t1 = "aardappelkop"

t2 :: String
t2 = "aardappelkop is een soort aardappel"

t3 :: String
t3 = "aardappelkop is een soort aardappel met een lange naam"

testKey :: String
testKey = "verysecretkeybro" --key moet 16 karakters lang zijn

intKey :: Key
intKey = string2ints testKey

testBlock :: Block
testBlock =
    [
        [1, 2, 3, 4],
        [5, 6, 7, 8],
        [9, 10, 11, 12],
        [13, 14, 15, 16] 
    ]

{-
Opdracht 2
Basisfuncties
-}
string2ints :: String -> [Int]
string2ints = map ord

ints2string :: [Int] -> String
ints2string = map chr

-- pad 5 [1,2,3,4,5,6,7]
-- [1,2,3,4,5,6,7,0,0,0]
pad :: Int -> [Int] -> [Int]
pad n xs = 
    let  r = length xs `mod` n
    in if r == 0 then xs else xs ++ replicate (n - r) 32

-- hack 4 [1,2,3,4,5,6,7,8,9,10,11,12]
-- [[1,2,3,4],[5,6,7,8],[9,10,11,12]]
hack :: Int -> [a] -> [[a]]
hack _ [] = []
hack n xs = take n xs : hack n (drop n xs)

type Block = [[Int]]
type Key = [Int]

-- Converteer String naar Blocks
-- blocked = string2blocks t1
-- blocked
string2blocks :: String -> [Block]
string2blocks input =
    let intList = string2ints input 
        padList = pad 16 intList
        blocks = hack 16 padList
    in map (hack 4) blocks

-- Converteer Blocks terug naar een String
-- unblocked = blocks2string blocked
-- unblocked
blocks2string :: [Block] -> String
blocks2string blocks = 
    let flatList = concatMap concat blocks
    in ints2string flatList


{-
Opdracht 3
Permutaties
-}
-- roteer een lijst met n elementen, lijst is altijd 4 elementen lang
rotatea :: Int -> [a] -> [a]
rotatea n xs = 
    let len = 4
        k = (n `mod` len)  -- Normalize n to be within [0, len-1]
    in drop k xs ++ take k xs

-- roteer een blok met 4 rijen
rotblock :: Block -> Block
rotblock [r1, r2, r3, r4] =
    [ r1 ,
    rotatea 1 r2,
    rotatea 2 r3,
    rotatea 3 r4
    ]

-- de inverse van rotblock
invrotblock :: Block -> Block
invrotblock [r1, r2, r3, r4] =
    [ r1 ,
    rotatea (-1) r2,
    rotatea (-2) r3,
    rotatea (-3) r4
    ]

-- transpose is van Data.List
-- transpose een matrix (lijst van lijsten)
-- transpose [[1,2,3],[4,5,6],[7,8,9]] = [[1,4,7],[2,5,8],[3,6,9]]
rotblockbycolumn :: Block -> Block
rotblockbycolumn block = transpose (rotblock (transpose block))

invrotblockbycolumn :: Block -> Block
invrotblockbycolumn block = transpose (invrotblock (transpose block))


{-
Opdracht 4
Sleutelgeneratie en Vernam cypher
-}
rotatelist :: [a] -> Int -> [a]
rotatelist xs n
    | null xs   = []
    | otherwise =
        let len = length xs
            n'  = (n `mod` len + len) `mod` len  -- Voorkomt negatieve indexen
        in drop n' xs ++ take n' xs

-- genereer een lijst van sleutels door de originele sleutel te roteren
genroundkeys :: Key -> Int -> [Key]
genroundkeys key n =
    let infices = [0 .. n - 1] -- genereer een lijst van indexen
        roundKeys = map (rotatelist key) infices -- roteer de sleutel voor elke index
    in roundKeys

-- maakt van een blok een lijst van integers
block2list :: Block -> [Int]
block2list = concat

-- maakt van een lijst van integers een blok
-- blockxorkey [1,2,3,4] intKey
blockxorkey :: Block -> Key -> Block
blockxorkey block key = 
    let flatBlock = concat block -- maak er een lijst van
        keyCycle = cycle key -- herhaal de key
        xorList = zipWith xor flatBlock keyCycle -- herhaal elk block element met de key
        newBlock = hack 4 xorList -- maak er weer een blok van
    in newBlock


{-
Opdracht 5
Substitutie
-}
-- voert vigenere uit op een lijst; zipWith (+) met herhalende key
-- vigenere [1,2,3] [4,5,6]
vigenere :: [Int] -> [Int] -> [Int]
vigenere key message = zipWith (+) message (cycle key)

-- de inverse van vigenere
-- invvigenere [1,2,3] [4,5,6]
invvigenere :: [Int] -> [Int] -> [Int]
invvigenere key message = zipWith (-) message (cycle key)

-- voert vigenere uit op een blok
-- vblock = vigenereblock testBlock intKey
vigenereblock :: Block -> Key -> Block
vigenereblock block key = 
    let flatBlock = concat block -- maak er een lijst van
        keyCycle = cycle key -- herhaal de key
        vigenereList = vigenere keyCycle flatBlock -- herhaal elk block element met de key
        newBlock = hack 4 vigenereList -- maak er weer een blok van
    in newBlock

-- de inverse van vigenereblock
-- invblock = invvigenereblock vblock intKey
invvigenereblock :: Block -> Key -> Block
invvigenereblock block key =
    let flatBlock = concat block -- maak er een lijst van
        keyCycle = cycle key -- herhaal de key
        invVigenereList = invvigenere keyCycle flatBlock -- herhaal elk block element met de key
        newBlock = hack 4 invVigenereList -- maak er weer een blok van
    in newBlock



{-
Opdracht 6
Encryptiefunctie
-}
encryptround :: Block -> Key -> Block
encryptround block key =
    let step1 = rotblock block
        step2 = rotblockbycolumn step1
        step3 = vigenereblock step2 key
        step4 = blockxorkey step3 key
    in step4

decryptround :: Block -> Key -> Block
decryptround block key =
    let step1 = blockxorkey block key
        step2 = invvigenereblock step1 key
        step3 = invrotblockbycolumn step2
        step4 = invrotblock step3
    in step4

encryptrounds :: Key -> Block -> Block
encryptrounds key block =
    let roundKeys = genroundkeys key 10
    in foldl encryptround block roundKeys

decryptrounds :: Key -> Block -> Block
decryptrounds key block =
    let roundKeys = reverse (genroundkeys key 10)
    in foldl decryptround block roundKeys

encrypt :: String -> String -> String
encrypt message key = 
    let keyInts = string2ints key
        blocks = string2blocks message
        encryptedMessage = map (encryptrounds keyInts) blocks
    in blocks2string encryptedMessage


decrypt :: String -> String -> String
decrypt message key = 
    let keyInts = string2ints key
        blocks  = string2blocks message
        decryptedMessage = map (decryptrounds keyInts) blocks
    in blocks2string decryptedMessage


{-
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
TestBLOCK
let encrypted1 = encrypt t1 testKey
let encrypted2 = encrypt t1 testKey
encrypted1 == encrypted2

let encrypted1 = encrypt "Hello Long" testKey
let encrypted2 = encrypt "Hello Lone" testKey
encrypted1 == encrypted2


-}