{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module RomaniukA05 where

type Grammar = [Production]
type Production = (Char,String)
-- Граматика - список продукцій.
--   нетермінал першої - початковий

-- Лівосторонній вивід - послідовність слів  [String] з іншого боку
--     послідовність номерів правил, які при цьому застосовувались [Int]
type DerivationS = [String]
type DerivationR = [Int]

reverseList :: [a] -> [a]
reverseList a | null a = []
              | otherwise = last a : reverseList (init a)

-- Задача 1 -----------------------------------------
isNonTerminal :: Char -> Bool
isNonTerminal char = char `elem` "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

isProduction :: Production -> Bool
isProduction pr = ((length (snd pr) /= 1) || (fst pr /= head (snd pr)))
                  && isNonTerminal (fst pr) && '$' `notElem` snd pr

isGrammar ::  Grammar -> Bool
isGrammar gr = and (map isProduction gr)

-- Задача 2.a ---------------------------------------
allTerm :: Grammar -> String
allTerm gr = let a = [ y | z<-[snd x | x <- gr], y<-z, not (isNonTerminal y)]
            in reverseList [a!!x | x<-[0..length a-1], (a!!x) `notElem` take x a ]

-- Задача 2.b ---------------------------------------
allNotT :: Grammar -> String
allNotT gr = let a = [ y | z<-[[fst x] | x <- gr] ++ [snd x | x <- gr], y<-z, isNonTerminal y]
            in reverseList [a!!x | x<-[0..length a-1], (a!!x) `notElem` take x a ]

-- Задача 2.c ---------------------------------------
indexInList :: [Char] -> Char -> Int -> Int
indexInList xs x curr
  | null xs = -1
  | head xs == x = curr
  | otherwise = indexInList (tail xs) x (curr+1)

maxValue :: [Int] -> Int
maxValue (x:xs)   | null xs = x
                  | x >= maxValue xs = x
                  | otherwise = maxValue xs

newMinN :: Grammar -> Char
newMinN gr = head [x | x <- "ABCDEFGHIJKLMNOPQRSTUVWXYZ", x `notElem` allNotT gr]

-- Задача 3.a -----------------------------------------
areNotCrossReferencing :: Char -> Char -> Grammar -> Bool
areNotCrossReferencing a b gr = or [ b == fst y && a `notElem` snd y && b `notElem` snd y | y<-gr]
                              || or [ a == fst y && b `notElem` snd y && a `notElem` snd y | y<-gr]

isProductive :: Grammar -> Char -> Bool
isProductive gr nt = (or a)
                  && (or (map (\x -> and [not (isNonTerminal z) | z<-x]) [snd y | y<-gr, nt == fst y])
                  || or [isProductive gr z | y<-gr, z<-snd y, nt == fst y, nt /= z, isNonTerminal z, areNotCrossReferencing nt z gr])
            where a = [nt == fst x && nt `notElem` snd x | x<-gr]

buildGen :: Grammar -> String
buildGen gr = filter (isProductive gr) (allNotT gr)

-- Задача 3.b -----------------------------------------
isReachable :: Grammar -> Char -> Bool
isReachable gr p  | p == initNonT = True
                  | otherwise = or [initNonT == fst pr && p `elem` snd pr| pr<-gr]
                  || ( or [ p /= fst pr && p `elem` snd pr | pr<-gr, areNotCrossReferencing p (fst pr) gr]
                        && isReachable gr (head [ fst pr | pr<-gr, p /= fst pr && p `elem` snd pr]))
      where initNonT = fst (head gr)

buildAcc :: Grammar -> String
buildAcc gr = filter (isReachable gr) (allNotT gr)

-- Задача 3.c -----------------------------------------
removeNotions :: Grammar -> Char -> Grammar
removeNotions gr nt = [pr | pr<-gr, nt /= fst pr && nt `notElem` snd pr]

reduce :: Grammar -> Grammar
reduce gr   | null a = gr
            | not (isProductive gr initNonT) = []
            | otherwise = reduce (removeNotions gr (head a))
      where a = [t | t<-allNotT gr, not (isProductive gr t && isReachable gr t), t /= initNonT]
            initNonT = fst (head gr) 

-- Задача 4.a -----------------------------------------
findLeftR :: Grammar -> String
findLeftR gr = [nt | nt<-allNotT gr, or [nt == fst pr && nt == head (snd pr) | pr<-gr],
                                    or [nt == fst pr && nt /= head (snd pr) | pr<-gr]]

-- Задача 4.b -----------------------------------------
deleteLeftR :: Grammar -> Char -> Grammar
deleteLeftR gr nt = [pr | pr<-gr, nt /= fst pr]
                  ++ [(nt, re ++ [newMin]) | re<-allEndings]
                  ++ [(newMin, y++[newMin]) | y<-allReccursions] ++ [(newMin, "")]
            where newMin = newMinN gr
                  allEndings = [snd y | y<-gr, nt == fst y, nt /= head (snd y)]
                  allReccursions = [tail (snd y) | y<-gr, nt == fst y, nt == head (snd y)]
-- Задача 5.a -----------------------------------------
isFact :: Grammar -> Char -> Bool
isFact gr nt = or [nt == fst x && nt == fst y && head (snd x) == head (snd y) | x<-gr, y<-gr, x/=y]

-- Задача 5.b -----------------------------------------
hasPrefix :: String -> String -> Bool
hasPrefix st pre  | null pre = True
                  | length pre > length st = False
                  | otherwise = head st == head pre && hasPrefix (tail st) (tail pre)

deleteFact :: Char -> String -> Grammar -> Grammar
deleteFact nt pre gr = [pr | pr<-gr, not (fst pr == nt && snd pr `hasPrefix` pre)]
                        ++ [(nt,pre++[newMin])]
                        ++ [(newMin, y) | y<-allSuffixes]
      where newMin = newMinN gr
            allSuffixes = [drop (length pre) (snd y) | y<-gr, fst y == nt, snd y `hasPrefix` pre]
            --allWithoutPrefix = [snd y | y<-gr, fst y == nt, not (snd y `hasPrefix` pre)]

-- Задача 6.a -----------------------------------------
replace :: String -> Production -> String
replace st pr
  | null st = []
  | head st == fst pr = snd pr ++ tail st
  | otherwise = head st : replace (tail st) pr

isDerivableS :: Grammar -> String -> String -> Bool
isDerivableS gr wr1 wr2 = or [replace wr1 pr == wr2 | pr<-gr ]

isLeftDerivationS :: Grammar -> DerivationS -> Bool
isLeftDerivationS gr der      | null (tail der) = True
                              | otherwise = isDerivableS gr (head der) (head (tail der))
                              && isLeftDerivationS gr (tail der)

-- Задача 6.b -----------------------------------------
isTerminal :: String -> Bool
isTerminal st = and [not (isNonTerminal x) | x<-st] 

isLeftDerivationRHelper :: Grammar -> DerivationR -> String -> Bool
isLeftDerivationRHelper gr der st   | null der = isTerminal st
                                    | otherwise = isLeftDerivationRHelper gr (tail der) (st `replace` (gr !! head der))

isLeftDerivationR :: Grammar -> DerivationR -> Bool
isLeftDerivationR gr der = isLeftDerivationRHelper gr der [initNonT]
      where initNonT = fst (head gr) 

-- Задача 7 -----------------------------------------
fromLeftRHelper :: Grammar -> DerivationR -> String -> DerivationS
fromLeftRHelper gr der help   | null der = [help]
                              | otherwise = help : fromLeftRHelper gr (tail der) a
      where a = replace help (gr !! head der)

fromLeftR :: Grammar -> DerivationR -> DerivationS
fromLeftR gr der = fromLeftRHelper gr der [initNonT]
      where initNonT = fst (head gr) 
--------------------------------------------------------
--  тестові дані 
gr0, gr1, gr1e, gr2 :: Grammar
gr0 = [('S',"aAS"), ('S',"a"),('A',"SbA"),('A',"ba")]
gr1 = [ ('S',"aSa"), ('S',"bSd"), ('S',"c"), ('S',"aSb"), ('D',"aC")
      , ('A',"cBd"), ('A',"aAd"),('B',"dAf"),('C',"cS"), ('C',"a")]
gr1e = [('S',"aAS"), ('S',"a"),('a',"SbA"),('A',"ba"),('S',"")]
gr2 = [('E',"E+T"),('E',"T"), ('T',"T*F"), ('T',"F"), ('F',"d"),('F',"(E)") ]

gr0S, gr0Se :: [String]
gr0S = ["S", "aAS", "aSbAS", "aabAS", "aabbaS", "aabbaa"]
gr0Se = ["S", "aAS", "aSbAS", "aabAS", "aabbaS", "aabba"]

gr0R :: DerivationR
gr0R = [0, 2, 1, 3, 1]



