module Encryption where
import Data.Bits
import Data.Char
import Data.List

t1 = "simpele tekst"
t2 = "dit is een grotere teststring om te testen of encryptie werkt"
t3 = "Binnen de cryptografie staat encryptie of versleuteling voor het omzetten van een bericht als leesbare tekst de klare tekst naar de versleutelde tekst het geheimschrift ook wel als cijfertekst aangeduid Dit versleutelen is al een zeer oud gebruik Een versleuteling die met de hand kan worden uitgevoerd noemt men een handcijfer maar tegenwoordig gebeurt het versleutelen en ontsleutelen meestal met behulp van een computer De cijfertekst kan nadien weer worden ontsleuteld zodat men de originele klare tekst weer terugkrijgt Dit proces wordt decryptie genoemd Een methode van encryptie wordt wel een cijfer genoemd"

testkey = "hieriseentestkey"

type Block = [[Int]]
type Key = [Int]

string2Ints::String->[Int]
string2Ints _str =  (map ord _str)

ints2String::[Int]->String
ints2String _ints = map chr _ints

pad::Int->[Int]->[Int]
pad n xs = 
    let  r = length xs `mod` n
    in if r == 0 then xs else xs ++ replicate (n - r) 32

hack::Int->[a]->[[a]]
hack _amount _arr | _amount > 0 = [drop x (take (_amount+x) _arr) | x <- [0,_amount..(length _arr - 1)]]

string2blocks::String->[Block]
string2blocks _str = hack 4 (hack 4 (pad 16 (string2Ints _str)))

blocks2string::[Block]->String
blocks2string _blocks = ints2String (concat (concat _blocks))

rotatea::Int->[a]->[a]
rotatea _amount _arr | (_amount) `mod` (length _arr) == 0 = _arr | (_amount) `mod` (length _arr) < ((length _arr) `div` 2) = rotatea (((_amount) `mod` (length _arr))-1) ((tail _arr) ++ [(head _arr)]) | (_amount) `mod` (length _arr) >= ((length _arr) `div` 2) = rotatea (((_amount) `mod` (length _arr))+1) ((drop (length _arr - 1) _arr) ++ (take (length _arr - 1) _arr))

rotblock::Block->Block
rotblock _block = [rotatea x (head (drop x _block)) | x <- [0..((length _block) - 1)]]

invrotblock::Block->Block
invrotblock _block = [rotatea (-x) (head (drop x _block)) | x <- [0..((length _block) - 1)]]

rotblockbycolumn::Block->Block
rotblockbycolumn _block = [[head (drop w (head (drop z [rotatea y [head (drop y (head (drop x _block))) | x <- [0..(length _block - 1)]] | y <- [0..(length (head _block) - 1)]]))) | z <- [0..(length (head _block) - 1)]] | w <- [0..(length _block - 1)]]

invrotblockbycolumn::Block->Block
invrotblockbycolumn _block = [[head (drop w (head (drop z [rotatea (-y) [head (drop y (head (drop x _block))) | x <- [0..(length _block - 1)]] | y <- [0..(length (head _block) - 1)]]))) | z <- [0..(length (head _block) - 1)]] | w <- [0..(length _block - 1)]]

rotatelist::[a]->Int->[a]
rotatelist _list _amount | (_amount) `mod` (length _list) == 0 = _list | (_amount) `mod` (length _list) < ((length _list) `div` 2) = rotatelist ((tail _list) ++ [(head _list)]) (((_amount) `mod` (length _list))-1) | (_amount) `mod` (length _list) >= ((length _list) `div` 2) = rotatelist ((drop (length _list - 1) _list) ++ (take (length _list - 1) _list)) (((_amount) `mod` (length _list))+1)

genroundkeys::Key->Int->[Key]
genroundkeys _key _amount = [rotatelist _key x | x <- [0..(_amount-1)]]

block2list::Block->[Int]
block2list _block = concat _block

blockxorkey :: Block -> Key -> Block
blockxorkey block key = 
    let flatBlock = concat block -- maak er een lijst van
        keyCycle = cycle key -- herhaal de key
        xorList = zipWith xor flatBlock keyCycle -- herhaal elk block element met de key
        newBlock = hack 4 xorList -- maak er weer een blok van
    in newBlock


vigenere::[Int]->[Int]->[Int]
vigenere key message = zipWith (+) message (cycle key)

invvigenere::[Int]->[Int]->[Int]
invvigenere a b = [(head (drop x b)) - (head (drop x a)) | x <- [0..(length a - 1)]]

vigenereblock::Block->Key->Block
vigenereblock a b = map (vigenere b) a

invvigenereblock::Block->Key->Block
invvigenereblock a b = map (invvigenere b) a

encryptround::Block->Key->Block
encryptround a b = blockxorkey (vigenereblock (rotblockbycolumn (rotblock a)) b) b

decryptround::Block->Key->Block
decryptround a b = invrotblock (invrotblockbycolumn (invvigenereblock (blockxorkey a b) b))

encryptrounds::Key->Block->Block
encryptrounds a b = foldl encryptround b (genroundkeys a 10)

decryptrounds::Key->Block->Block
decryptrounds a b = foldl decryptround b (reverse (genroundkeys a 10))

encrypt::String->String->String
encrypt a b = blocks2string (map (encryptrounds (string2Ints b)) (string2blocks a))

decrypt::String->String->String
decrypt a b = blocks2string (map (decryptrounds (string2Ints b)) (string2blocks a))


testKey::String
testKey = "verysecretkeybro" --key moet 16 karakters lang zijn
testBlock::Block
testBlock = [[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]]
intKey::Key
intKey = string2Ints testKey