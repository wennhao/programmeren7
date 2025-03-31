module Opdracht3 where
import Data.Bits
import Data.Char
import Data.List
import GHC.Exts.Heap (GenClosure(key))
import Debug.Trace (trace)


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
testKey = "verysecretkeybro" --key moet 16 karakters lang zijn


{-
Opdracht 2
Basisfuncties
-}
string2ints :: String -> [Int]
string2ints = map ord

ints2string :: [Int] -> String
ints2string = map chr

pad :: Int -> [Int] -> [Int]
pad n xs = 
    let r = length xs `mod` n
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
    , rotatelist r2 1
    , rotatelist r3 2
    , rotatelist r4 3
    ]
rotblock _ = error "Block moet 4 rijen hebben"

invrotblock :: Block -> Block
invrotblock [r1, r2, r3, r4] =
    [ r1
    , rotatelist r2 (-1)
    , rotatelist r3 (-2)
    , rotatelist r4 (-3)
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
rotatelist xs n
    | null xs   = []
    | otherwise =
        let len = length xs
            n'  = (n `mod` len + len) `mod` len  -- Voorkomt negatieve indexen
        in drop n' xs ++ take n' xs

genroundkeys :: Key -> Int -> [Key]
genroundkeys key n = [rotatelist key i | i <- [0 .. n-1]]

block2list :: Block -> [Int]
block2list = concat

blockxorkey :: Block -> Key -> Block
blockxorkey block key = hack 4 (zipWith xor (concat block) (cycle key))


{-
Opdracht 5
Substitutie
-}
-- Tel de elementen van twee lijsten op.
vigenere :: [Int] -> [Int] -> [Int]
vigenere key msg = zipWith (+) msg (cycle key)

-- Tel de elementen van twee lijsten op
invvigenere :: [Int] -> [Int] -> [Int]
invvigenere key msg = zipWith (-) msg (cycle key)

vigenereblock :: Block -> Key -> Block
vigenereblock block key = hack 4 (vigenere (cycle key) (block2list block))


invvigenereblock :: Block -> Key -> Block
invvigenereblock block key = hack 4 (invvigenere (cycle key) (block2list block))



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
    in foldl (\acc key -> decryptround acc key) block roundKeys

encrypt :: String -> String -> String
encrypt message key = 
    let keyInts = string2ints key
        blocks  = string2blocks message
        enc     = map (encryptrounds keyInts) blocks
    in blocks2string enc


decrypt :: String -> String -> String
decrypt message key = 
    let keyInts = string2ints key
        blocks  = string2blocks message
        dec     = map (decryptrounds keyInts) blocks
    in blocks2string dec


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