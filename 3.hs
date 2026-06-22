{-# OPTIONS_GHC -Wall #-}
module RomaniukA10A where

type PolinomOne = [(Int,Rational)]
type Linear   = [Row]
type Row      = [Rational]
data Solution = Empty | One Row  | Many [PolinomOne]
                 deriving (Show, Eq)

-- Задача 1.a -----------------------------------------
coef :: Rational -> PolinomOne -> PolinomOne
coef 0 _ = []
coef num polin = [(x,num*y) | (x, y)<-polin]

-- Задача 1.b -----------------------------------------
-- getNthPolyCoef :: PolinomOne -> Int -> Rational
-- getNthPolyCoef poly index = [ y | (x, y)<-poly, x == index]

add :: PolinomOne -> PolinomOne -> PolinomOne
add a [] = a
add [] a = a
add xyz@((x,y):xy) abc@((a,b):ab)
  | x == a = if y+b == 0 then add xy ab else (x, y+b):add xy ab
  | x < a = (x,y):add xy abc
  | otherwise = (a,b):add xyz ab

-- Задача 1.c -----------------------------------------
removeSecondZeros :: PolinomOne -> PolinomOne
removeSecondZeros [] = []
removeSecondZeros (xs@(_, b):xss)   | b == 0 = removeSecondZeros xss
                                    | otherwise = xs : removeSecondZeros xss

unify :: PolinomOne -> PolinomOne
unify [] = []
unify ((x, y):poli) = removeSecondZeros (add [(x, y)] (unify poli))

-- Задача 2.a -----------------------------------------
isFreePoli :: PolinomOne -> Int -> Bool
isFreePoli xs ind
  | length xs /= 1 = False
  | fst (head xs) == ind = True
  | otherwise = False

findFree :: [PolinomOne] -> [Int]
findFree xs = [ind+1 | ind<-[0..length xs-1], isFreePoli (xs!!ind) (ind+1)]

-- Задача 2.b -----------------------------------------
reliesOn :: PolinomOne -> [Int] -> Bool
reliesOn poli ints = and [ x `elem` ints || x == 0 | (x, _)<-poli]

iswfCommon ::  [PolinomOne]  -> Bool
iswfCommon polis = and [reliesOn (polis!!ind) freeOnes | ind<-[0..length polis-1], (ind+1) `notElem` freeOnes]
        where freeOnes = findFree polis

-- Задача 3.a -----------------------------------------
allLengthOne :: [[a]] -> Bool
allLengthOne = foldr (\ x -> (&&) (length x == 1)) True

removeZeros :: [Rational] -> [Rational]
removeZeros [] = []
removeZeros xss@(x:xs)  | x == 0 = removeZeros xs
                        | otherwise = xss

isSimple :: Linear -> Bool
isSimple = allLengthOne

-- Задача 3.b -----------------------------------------
allSameValue :: Linear -> Bool
allSameValue = foldr (\ x -> (&&) (head x == 0)) True


solveSimple :: Linear -> Maybe [PolinomOne]
solveSimple line = if allSameValue line then Just []
                    else Nothing

-- Задача 4.a -----------------------------------------
allValueZero :: [Rational] -> Bool
allValueZero [] = True
allValueZero (x:xs) = x == 0 && allValueZero xs

findRow :: Linear -> Maybe Int
findRow liness = let ints = [head line | line<-liness] in
                    if allValueZero ints then Nothing else
                        Just (head [ x+1 | x<-[0..length ints-1], (ints!!x) /= 0])

-- Задача 4.b -----------------------------------------
exchangeRow :: [a] -> Int -> [a]
exchangeRow rows ind = [ if (curr+1) == ind
                            then head rows
                            else if curr==0
                                then rows!!(ind-1)
                                else rows!!curr  | curr<-[0..length rows-1]]

-- Задача 5.a -----------------------------------------
forwardStep :: Row -> Linear -> Linear
forwardStep mainRow line = [ [ let  ajk = row!!ind
                                    aji = head row
                                    a1k = mainRow!!ind in
                                    ajk - (aji/a1i)*a1k| ind<-[1..length row-1]] | row<-line]
                where a1i = head mainRow

-- Задача 5.b -----------------------------------------
reverseStep :: Row -> [PolinomOne] -> [PolinomOne]
reverseStep row polis = coef (1/head row) (add [(0, last row)] (foldr1 add [ coef (-row!!(ind+1)) (polis!!ind) | ind<-[0..length polis-1]])):polis

-- Задача 6 -----------------------------------------
gauss :: Int -> Linear -> Maybe [PolinomOne]
gauss _ [] = Just [[]]
gauss i liners
  | isSimple liners = solveSimple liners
  | otherwise = case findRow liners of
        Nothing -> let reduced = map tail liners in
                        case gauss (i+1) reduced of
                            Nothing -> Nothing
                            Just b -> Just ([(i, 1)]:b)
        Just b -> let   echanged = exchangeRow liners b
                        heads = head echanged
                        tails = tail echanged
                        forwarded = forwardStep heads tails in
                        case gauss (i+1) forwarded of
                            Nothing -> Nothing
                            Just [[]] -> Just (init (reverseStep heads [[]]))
                            Just a -> Just (reverseStep heads a)

-- Задача 7.a -----------------------------------------
testEquation :: [PolinomOne] -> Row -> Bool
testEquation pos row = (foldr1 add [ coef (row!!ind) (pos!!ind) | ind<-[0..length args-1]]) == [(0,b)]
                where   args = init row
                        b = last row

-- Задача 7.b -----------------------------------------
testLinear :: [PolinomOne] -> Linear -> Bool
testLinear _ [] = True
testLinear polis liners = testEquation polis (head liners)
                            && testLinear polis (tail liners)

-- Задача 8 -----------------------------------------
solving :: Linear -> Solution
solving liners = case gauss 1 liners of
                    Nothing -> Empty
                    Just [] -> One []
                    Just a -> if iswfCommon a
                        then Many a
                        else One ([snd (head x) | x<-a])

-------------------------------------------------------
pol0, pol1, pol2, pol3, pol4 :: PolinomOne
pol0 = [(0,3/5), (3,1), (3,-2/7), (2,3), (0,-7/3), (4,0)]
pol1 = [(5,3/4), (0,7), (4,3/2), (5,-2/3), (0,1/2)]
pol2 = [(0,15), (4,3),(5,1)]
pol3 = [(0,-10), (2,7), (4,-3)]
pol4 = [(0,-26/15), (2,3), (3,5/7)]

test0, test1, test2, test3, test3a, test4 :: Linear
test0 = [[0,-2,-1,2],[0,-4,-5,3],[1,2,4,5]]
test1 = [[4,-3,2,-1,8],[3,-2,1,-3,7],[5,-3,1,-8,1]]
test2 = [[7,-2,-1,2],[6,-4,-5,3],[1,2,4,5]]
test3 = [[2,3,-1,1,1],[8,12,-9,8,3],[4,6,3,-2,3],[2,3,9,-7,3]]
test3a = [[0,-5,4,-1], [0,5,-4,1],[0,10,-8,2]]
test4 = [[6,1,2,21], [4,-6,16,2], [3,8,1,2]]

res3, res4 :: [PolinomOne]
res3 = [[(0,3/5),(2,-3/2),(4,-1/10)],[(2,1)],[(0,1/5),(4,4/5)],[(4,1)]]
res4 = [[(0,62/15)], [(0,-17/15)], [(0,-4/3)]]

sol1,sol2,sol3,sol4 :: Solution
sol1 = Empty
sol2 = Empty
sol3 = Many res3
sol4 = One [62/15,-17/15,-4/3]


