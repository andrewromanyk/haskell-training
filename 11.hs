{-# OPTIONS_GHC -Wall #-}
module RomaniukA07 where

newtype Poly a = P [a]

-- Задача 1 ----------------------------------------
shorten :: (Num a, Eq a) => [a] -> [a]
shorten b   | null b = [0]
            | last b == 0 = shorten (init b)
            | otherwise = b

instance (Num a, Eq a) => Eq (Poly a) where
    (==) (P b) (P c) = shorten b == shorten c

-- Задача 2 -----------------------------------------
showXthPower :: (Num a, Eq a, Show a, Num b, Eq b, Show b) => a -> b -> String
showXthPower 0 _ = ""
showXthPower d 0 = show d
showXthPower 1 1 = "x"
showXthPower (-1) 1 = "-x"
showXthPower 1 d = "x^"++show d
showXthPower (-1) d = "-x^"++show d
showXthPower d 1 = show d ++ "x"
showXthPower c d = show c ++ "x^" ++ show d


instance (Num a, Eq a, Show a) => Show (Poly a) where
    show :: (Num a, Eq a, Show a) => Poly a -> String
    show (P b)  | null b || (length shortB == 1 && head shortB == 0) = "0"
                | otherwise = let shortenedArr = reverse shortB in
                    foldl1 (\x y -> if y == "" then x else x ++ " + " ++ y) [showXthPower (shortenedArr !! x) (length shortB-1-x) | x <- [0..length shortB-1]]
                where shortB = shorten b


-- Задача 3 -----------------------------------------
addArrays :: (Num a) => [a] -> [a] -> [a]
addArrays (x:xs) (y:ys) = (x+y) : addArrays xs ys
addArrays [] ys = ys
addArrays xs [] = xs

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P b) (P c) = P (addArrays b c)

-- Задача 4 -----------------------------------------
arrayWithNZeros :: (Num a) => Int -> [a]
arrayWithNZeros 0 = []
arrayWithNZeros b = 0 : arrayWithNZeros (b-1)

multipliedBy :: (Num a) => [a] -> a -> [a]
multipliedBy b c = map (*c) b

multiplyArrays :: (Num a) => [a] -> [a] -> [a]
multiplyArrays b c  | null b || null c = [0]
                    | otherwise = foldl1 addArrays [arrayWithNZeros x ++ c `multipliedBy` (b!!x) | x <- [0..length b - 1]]

times :: Num a => Poly a -> Poly a -> Poly a
times (P b) (P c) = P (multiplyArrays b c)

-- Задача 5 -----------------------------------------
instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P b) = P (map (*(-1)) b)
    fromInteger b = P [fromInteger b]
    -- Розумних означень не існує
    abs    = undefined
    signum = undefined

-- Задача 6 -----------------------------------------
pow :: Num a => a -> Int -> a
pow _ 0 = 1
pow a b = a*pow a (b-1)

applyP :: Num a => Poly a -> a -> a
applyP (P xs) b = sum [ (xs!!x) * pow b x | x<-[0..length xs - 1]]

-- Задача 7 -----------------------------------------
class Num a => Differentiable a where
    derive  :: a -> a
    nderive :: Int -> a -> a
    nderive 0 f = f
    nderive n f = nderive (n-1) (derive f)

-- Задача 8 -----------------------------------------
instance Num a => Differentiable (Poly a) where
    derive (P []) = P [0]
    derive (P a) = P (tail [ (a!!x) * fromInteger (toInteger x) | x <- [0..length a-1]])
