{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module RomaniukA03 where

-- Задача 1 -----------------------------------------
expPart :: Integer -> Integer -> Double
expPart m n = sum (scanl (\x y -> x*(fromIntegral m)/y) (fromIntegral m) [2..(fromIntegral n)])

-- Задача 2 -----------------------------------------
piramid :: [Integer]
piramid = [sum [n*n | n<-[1..k]] | k<-[1..]]

-- Задача 3 -----------------------------------------
testing :: [Int] -> Bool
testing xs  | null xs = True
            | otherwise = null (tail xs) || ((head xs <= head (tail xs)) && testing (tail xs))

-- Задача 4 -----------------------------------------
--Helper--
isprime :: Int -> Bool
isprime x = (x /= 1) && null [y | y<-[2..x-1], mod x y == 0]
--Helper--

primeCnt :: [Int] -> Int
primeCnt xs = length (filter (\x -> x>0 && isprime x) xs)

-- Задача 5 ----------------------------------------- 
compress :: [Int] -> [Int]
compress (x:xs) | null xs = [x]
                | x == head xs = compress xs
                | otherwise = x : compress xs

-- Задача 6 -----------------------------------------
--Helper--
primes :: Int -> Int -> [Int]
primes m n  | m == 1 = []
            | mod m n == 0 = n : primes (div m n) n 
            | otherwise = primes m (n+1)
--Helper--

primeFactor :: Int -> [Int]
primeFactor n = primes n 2

-- Задача 7 -----------------------------------------
intToString :: Int -> Int -> String
intToString n m | m == 0 = ""
                | otherwise = intToString n (div m n) ++ ["0123456789abcdef" !! mod m n]

-- Задача 8 -----------------------------------------
--Helper--
isPalindrom :: String -> Bool
isPalindrom (x:xs)  | null xs = True
                    | length xs == 1 = x == last xs
                    | otherwise = x == last xs && isPalindrom (init xs)
--Helper--
                
sumPalindrom2 :: Integer -> Integer
sumPalindrom2 n = sum [x | x<-[1..n], isPalindrom (intToString 2 (fromIntegral x))]
