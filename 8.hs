{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module RomaniukA04 where

type Graph  = [[Int]]

-- Задача 1 ------------------------------------
-- Helper
noDuplicates :: [Int] -> Bool
noDuplicates xs | null xs = True
                | otherwise = (null [x | x <- tail xs, x == head xs]) && noDuplicates (tail xs)

hasOnlySmaller :: [Int] -> Int -> Bool
hasOnlySmaller xs x | not (null xs) = head xs >= 0 && x > head xs && hasOnlySmaller (tail xs) x
                    | otherwise = True
-- Helper

isGraph :: Graph -> Bool
isGraph gr = and [noDuplicates x && x `hasOnlySmaller` length gr | x <- gr]

-- Задача 2 ------------------------------------
-- Helper
xor :: Bool -> Bool -> Bool
xor a b = (a && not b) || (not a && b)

verticeInGraph :: Int -> Int -> Graph -> Bool
verticeInGraph x y gr = x `elem` (gr !! y)
-- Helper

isTournament :: Graph -> Bool
isTournament gr = and [xor (verticeInGraph x y gr) (verticeInGraph y x gr) | x <- [0..length gr-1], y <- [0..length gr-1], x /= y]

-- Задача 3 ------------------------------------
isTransitive :: Graph -> Bool
isTransitive gr = and [verticeInGraph x z gr | x <- [0..length gr-1], y <- [0..length gr-1], z <- [0..length gr-1], verticeInGraph x y gr && verticeInGraph y z gr]

-- Задача 4 ------------------------------------
-- Helper
condW :: [[[Int]]] -> Bool
condW wss = null (head wss)

stepW :: Graph -> [[[Int]]] -> [[[Int]]]
stepW gr wss@(wsn:_) = [t:w | w@(x:xs) <- wsn, x `notElem` xs, t<- gr!!x] : wss
stepW _ []  = error "allWays:stepW"

allWays :: Graph -> Int -> [[[Int]]]
allWays gr v = until condW (stepW gr) [[[v]]]

longest :: [[t]] -> [t]
longest [] = []
longest (x:xs) = let s = longest xs
                    in if length s >= length x then s else x

reverseList :: [t] -> [t]
reverseList x = if null x then [] else last x : reverseList (init x)
-- Helper

longWay :: Graph -> Int -> Int -> Maybe [Int]
longWay gr a b = let xs = [y | x <- allWays gr a, y <- x, head y == b, noDuplicates y]
                    in if null xs then Nothing else Just . reverseList . longest $ xs

-- Задача 5 ------------------------------------
-- Helper
takeJust :: Maybe x -> x
takeJust (Just x) = x

longWayDupl :: Graph -> Int -> Int -> Maybe [Int]
longWayDupl gr a b = let xs = [y | x <- allWays gr a, y <- x, head y == b]
                    in if null xs then Nothing else Just . reverseList . longest $ xs
-- Helper

gamiltonWay :: Graph -> Maybe [Int]
gamiltonWay gr = if null a then Nothing else head a
                    where a = filter (\x -> length (takeJust x)-1 == length gr) ([longWayDupl gr x x | x <- [0..length gr - 1]])

-- Задача 6 ------------------------------------
isAcyclic :: Graph -> Bool
isAcyclic gr = and (map (\x -> 1==length (takeJust x)) [longWayDupl gr x x | x <- [0..length gr-1]])

-- Задача 7 ------------------------------------
-- Helper
findLast :: Graph -> Int -> [Int] -> Int
findLast gr x ign = if (not . null $ b) || x `elem` ign
                    then findLast gr (if null b then x+1 else head b) ign
                    else x
    where b = [a | a <- [0..length gr-1], a `elem` (gr !! x)]

removeNotions :: Graph -> Int -> Graph
removeNotions gr a = [if a == x then [] else filter (/=a) (gr !! x) | x <- [0..length gr - 1]]

topolSortHelper :: Graph -> [Int] -> Maybe [Int]
topolSortHelper gr ign
  | length gr == length ign = Just []
  | otherwise = Just (takeJust (topolSortHelper b (a:ign)) ++ [a])
  where
      a = findLast gr 0 ign
      b = removeNotions gr a
-- Helper

topolSort :: Graph -> Maybe [Int]
topolSort gr
    | not (isAcyclic gr) = Nothing
    | otherwise = topolSortHelper gr []

-- Задача 8------------------------------------
isTopolSort :: Graph -> [Int] -> Bool
isTopolSort gr lst  | length lst /= length gr || not (isAcyclic gr) || not (noDuplicates lst) = False
                    | otherwise = and [(lst !! x) `notElem` (gr !! (lst !! y)) | x <- [0..length lst-1], y <- [x+1..length lst-1]]

---------------------Тестові дані - Графи -------

gr1, gr2, gr3, gr4:: Graph
gr1 = [[1,2,3],[2,3],[3,4],[4],[]]
gr2 = [[3,4],[0,3],[0,1,4],[2,4],[1]]
gr3 = [[1],[2],[3],[1],[0,3]]
gr4 = [[1,2,3],[1,2,3],[1,2,3],[1,2,3],[0,1,2,3]]
