{-# OPTIONS_GHC -Wall #-}
module RomaniukA02 where

-- Задача 1 -----------------------------------------
productFl :: [Int] -> Int
productFl = foldl (*) 1
  
-- Задача 2 ----------------------------------------- 
andFl :: [Bool] -> Bool
andFl = foldl (&&) True

-- Задача 3 -----------------------------------------
maximumFr :: [Int] -> Int
maximumFr = foldr1 (\x y -> if (x > y) then x else y)
-- foldr1 max --

-- Задача 4 -----------------------------------------
tails :: [Int] -> [[Int]]
tails = scanr (:) []

-- Задача 5 -----------------------------------------
allFirst :: [String] -> String
allFirst xss = map head (filter (not . null) xss) 
