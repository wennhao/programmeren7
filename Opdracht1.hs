module Opdracht1
where     
-- opdr1a
faca :: Int -> Int
faca 0 = 1                    
faca x = x * faca (x - 1)

-- opdr1b
facb :: Int -> Int
facb x
    | x <= 0 = 1
    | otherwise = (facb (x-1)) * x

-- opdr2a
nulpuntena :: Double -> Double -> Double -> [Double]
nulpuntena a b c = [(-1 * b + sqrt(b^2 +(-4 * a * c))) / (2 * a),(-1 * b - sqrt(b^2 +(-4 * a * c))) / (2 * a)]

-- opdr2b
nulpuntenb :: Double -> Double -> Double -> [Double]
nulpuntenb a b c
    | discriminant > 0 = [x1, x2]
    | discriminant == 0 = [x1]
    | otherwise = []
    where
        discriminant = b^2 - 4*a*c
        x1 = (-b + sqrt(discriminant)) / (2 * a)
        x2 = (-b - sqrt(discriminant)) / (2 * a)

dobbel2 = [[x,y]|x<-[1..6],y<-[1..6]] -- opdr2C
dobbel3 = [[x,y,x+y]|x<-[1..6],y<-[1..6]]