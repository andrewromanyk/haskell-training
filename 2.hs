{-# OPTIONS_GHC -Wall #-}
module Negrub10 where 

type PolinomOne = [(Int,Rational)]
type Linear   = [Row]
type Row      = [Rational]
data Solution = Empty | One Row  | Many [PolinomOne] 
                 deriving (Show, Eq)

-- Задача 1.a -----------------------------------------
coef :: Rational -> PolinomOne -> PolinomOne 
coef c p = [(i, c * a) | (i, a) <- p, c * a /= 0]

-- Задача 1.b -----------------------------------------
add :: PolinomOne -> PolinomOne -> PolinomOne
add [] p1 = p1
add p0 [] = p0
add ((i0, a0):xs0) ((i1, a1):xs1)
    | i0 < i1   = (i0, a0) : add xs0 ((i1, a1):xs1)
    | i1 < i0   = (i1, a1) : add ((i0, a0):xs0) xs1
    | otherwise =
        let aSum = a0 + a1
        in if aSum /= 0
           then (i0, aSum) : add xs0 xs1
           else add xs0 xs1

-- Задача 1.c -----------------------------------------
unify :: PolinomOne -> PolinomOne
unify [] = []
unify ((i, c):xs) =
    let
        totalCoeff = c + sum [c' | (i', c') <- xs, i' == i]
        rest = [(i', c') | (i', c') <- xs, i' /= i]
    in if totalCoeff /= 0
       then sortPolinom((i, totalCoeff) : unify rest)
       else sortPolinom(unify rest)

sortPolinom :: PolinomOne -> PolinomOne
sortPolinom [] = []
sortPolinom (x:xs) = sortPolinom [y | y <- xs, fst y <= fst x] ++ [x] ++ sortPolinom [y | y <- xs, fst y > fst x]

-- Задача 2.a -----------------------------------------
findFree :: [PolinomOne] -> [Int]
--findFree pos = [index | (index, [(_, 1)]) <- zip [0..] pos] -- Для нестандартного вигляду, індекси списку
findFree pos = [i | [(i, 1)] <- pos]

-- Задача 2.b -----------------------------------------
iswfCommon ::  [PolinomOne]  -> Bool 
iswfCommon pos = not (null freeIndices) && all isValid pos
  where
    freeIndices = findFree pos
    isValid [(_, 1)] = True
    isValid poly     = all (\(i, _) -> i `elem` freeIndices || i == 0) poly

-- Задача 3.a -----------------------------------------
isSimple :: Linear -> Bool
isSimple le = all ((== 1) . length) le

-- Задача 3.b -----------------------------------------
solveSimple :: Linear -> Maybe [PolinomOne]
solveSimple le
  | all ((== 0) . head) le = Just []
  | otherwise              = Nothing

-- Задача 4.a -----------------------------------------
findRow :: Linear -> Maybe Int
findRow le = case filter (\(_, row) -> head row /= 0) (zip [0..] le) of
               []         -> Nothing
               ((i, _):_) -> Just (i+1)

-- Задача 4.b -----------------------------------------
exchangeRow :: [a] -> Int -> [a]
exchangeRow le i
  | i < 1 || i > length le = []
  | otherwise               = (le !! (i - 1)) : (take (i - 2) (tail le)) ++ [head le] ++ drop i le

-- Задача 5.a -----------------------------------------
forwardStep :: Row -> Linear -> Linear
forwardStep fs rs = 
    let a1 = head fs
    in [tail $ zipWith (\a b -> a - m * b) row fs | row <- rs, let m = (head row) / a1]

-- Задача 5.b -----------------------------------------
reverseStep :: Row -> [PolinomOne] -> [PolinomOne]
reverseStep fs vs =
    let a1 = head fs
        rhs = last fs
        tailCoeffs = tail (init fs)
        
        substitutedPoly = foldr add [(0, rhs / a1)] $
            zipWith (\coeff poly -> coef (-coeff / a1) poly) tailCoeffs vs

        mainPoly = unify substitutedPoly
    in mainPoly : vs

-- Задача 6 -----------------------------------------
gauss :: Int -> Linear -> Maybe [PolinomOne]
gauss _ [] = Just []
gauss i le
  | isSimple le = solveSimple le
  | otherwise   = case findRow le of
      Nothing -> 
          if all ((== 0) . head) le then
              fmap ([(i, 1)] :) (gauss (i + 1) (map tail le)) 
          else Nothing
      Just r  -> 
          let le' = forwardStep (le !! (r - 1)) (removeAt (r - 1) le)
              restSolution = gauss (i + 1) le'
          in fmap (reverseStep (le !! (r - 1))) restSolution

removeAt :: Int -> [a] -> [a]
removeAt n xs = take n xs ++ drop (n + 1) xs

exchangeDummyRow :: Linear -> Linear
exchangeDummyRow le = le ++ [replicate (length (head le) - 1) 0 ++ [0]]

-- Задача 7.a -----------------------------------------
testEquation :: [PolinomOne] -> Row -> Bool 
testEquation pos row =
    unify (sumPolynomials pos (init row)) == [(0, last row)]
  where
    sumPolynomials :: [PolinomOne] -> [Rational] -> PolinomOne
    sumPolynomials polys coeffs = unify $ concat [coef c p | (c, p) <- zip coeffs polys]

-- Задача 7.b -----------------------------------------
testLinear :: [PolinomOne] -> Linear -> Bool 
testLinear pos le = all (testEquation pos) le

-- Задача 8 -----------------------------------------
solving :: Linear -> Solution  
solving le =
    case gauss 1 le1 of
      Nothing  -> Empty
      Just []  -> One []
      Just pos -> if iswfCommon pos
                    then Many pos
                    else One (map getConstant pos)
  where
    le1 = exchangeDummyRow le
    getConstant :: PolinomOne -> Rational
    getConstant = maybe 0 id . lookup 0

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
sol4 = One [62/15, -17/15, -4/3] 


