module Opdracht3 where

import Data.Bits
import Data.Char
import Data.List


t1 :: String
t1 = "Hello world!"

t2 :: String
t2 = "Hello world! I am the world!"

t3 :: String
t3 = "Hello world! I am the world! I love Haskell"

testKey :: String
testKey = "verysecretkey"



stringToInts :: String -> [Int]
stringToInt = map ord

intsToString :: [Int] -> String
intsToString = map chr


pad :: Int -> [Int] -> [Int]
pad n xs = 
    let r = lenth xs `mod` n
    in if r == 0 then xs else xs ++ replicate (n - r) 0
