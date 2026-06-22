{-# OPTIONS_GHC -Wall #-}
module RomaniukA01 where

-- Задача 1 -----------------------------------------
productMy :: [Int] -> Int
productMy xs = if null xs then 1 else head xs * productMy (tail xs)

-- Задача 2 -----------------------------------------
zipMy :: [Int] -> [Int] -> [(Int,Int)]
zipMy xs ys = if null xs || null ys then [] else (head xs, head ys) : zipMy (tail xs) (tail ys)

-- Задача 3 -----------------------------------------
takeMy :: Int -> [Int] -> [Int]
takeMy n xs = if n == 0 || null xs then [] else head xs : takeMy (n-1) (tail xs)

-- Задача 4 -----------------------------------------
lookMy :: Int -> [(Int,Int)] -> Int
lookMy x pxs = if null pxs then -1 else (if fst (head pxs) == x then snd (head pxs) else lookMy x (tail pxs))

-- Задача 5 -----------------------------------------
initMy :: [Int] -> [Int]
initMy xs = if null xs || null (tail xs) then [] else head xs : initMy (tail xs)
