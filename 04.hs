{-# OPTIONS_GHC -Wall #-}
module RomaniukA11 where

--  Пакет parsec може бути закритим (hidden), 
--  щоб відкрити його потрібно завантажити файл з опцією -package parsec
--  ghci xxxx.hs -package parsec // ghc xxxx.hs -package parsec 

import Text.Parsec.String
import Text.Parsec
    ( char, lower, between, chainl1, eof, (<|>), parse )   --- parse
import Data.Char(isLower)

data BExp = Bvalue Bool | Bvar Char | Not BExp | And BExp BExp | Or BExp BExp
            deriving (Eq, Ord, Show)
type Env = [(Char, Bool)]

type NodeId = Int
type BDDNode =  (NodeId, (Char, NodeId, NodeId))
type BDD = (NodeId, [BDDNode])

-- Задача 1 -----------------------------------------
getVarValue :: Env -> Char -> Bool
getVarValue env var | null env = error "no such variable"
                    | fst (head env) == var = snd (head env)
                    | otherwise = getVarValue (tail env) var

checkSatHelper :: [BDDNode] -> Env -> Int -> Bool
checkSatHelper _ _ 0 = False
checkSatHelper _ _ 1 = True
checkSatHelper a env ind = let  node = head [ nodes | nodes<-a, fst nodes == ind]
                                (_, (x, left, right)) = node
                                nodeValue = getVarValue env x in
                                    checkSatHelper a env (if not nodeValue
                                                            then left
                                                            else right)


checkSat :: BDD -> Env -> Bool
checkSat (b, a) env = checkSatHelper a env b

-- Задача 2 -----------------------------------------
getAllLeadingTo :: BDD -> Int -> [(BDDNode, Bool)]
getAllLeadingTo (a, b) ind  | a == ind = []
                            | otherwise = [ (node, value)
                                                | node@(_, (_, left, right))<-b,
                                                value<- if left == ind && right == ind
                                                            then [False, True]
                                                            else if left == ind
                                                                then [False]
                                                                else ([True | right == ind])
                                                ]

satHelper :: BDD -> Int -> [[(Char, Bool)]]
satHelper bdd@(a, _) ind    | a == ind = [[]]
                            | otherwise = let nodes = getAllLeadingTo bdd ind in
                                            [ (name,value):derived |
                                            ((ind2, (name, _, _)), value) <- nodes,
                                            derived<-satHelper bdd ind2 ]


sat :: BDD -> [[(Char, Bool)]]
sat bdd = satHelper bdd 1

-- Задача 3 -----------------------------------------
--  Bvalue Bool | Bvar Char | Not BExp | And BExp BExp | Or BExp BExp
simplify :: BExp -> BExp
simplify (Or (Bvalue False) a@(Bvalue _)) = a
simplify (Or a@(Bvalue _) (Bvalue False)) = a
simplify (Or (Bvalue True) (Bvalue _)) = Bvalue True
simplify (Or (Bvalue _) (Bvalue True)) = Bvalue True
simplify (And (Bvalue True) a@(Bvalue _)) = a
simplify (And a@(Bvalue _) (Bvalue True)) = a
simplify (And (Bvalue False) (Bvalue _)) = Bvalue False
simplify (And (Bvalue _) (Bvalue False)) = Bvalue False
simplify (Not (Bvalue a)) = Bvalue (not a)
simplify a = a

-- Задача 4 -----------------------------------------
restrict :: BExp -> Char -> Bool -> BExp
restrict a@(Bvalue _) _ _ = a
restrict a@(Bvar var) chartofind val = if var == chartofind then Bvalue val else a
restrict (Not var) chartofind val = simplify (Not (restrict var chartofind val))
restrict (And var1 var2) chartofind val = simplify (And (restrict var1 chartofind val) (restrict var2 chartofind val))
restrict (Or var1 var2) chartofind val = simplify (Or (restrict var1 chartofind val) (restrict var2 chartofind val))

-- Задача 5 -----------------------------------------
-- Передумова: Кожна змінна (буква) в булевому виразі (BExp) з"являється 
--    точно один раз в списку змінних (Char); немає інших елементів
buildBDD :: BExp -> [Char] -> BDD
buildBDD e xs = buildBDD' e 2 xs

buildBDD' :: BExp -> NodeId -> [Char] -> BDD
buildBDD' e _ [] = case e of
                    Bvalue False -> (0, [])
                    Bvalue True -> (1, [])
                    _ -> error "wrong expression"
buildBDD' e n (x:xs) = let  firstFalse = restrict e x False
                            firstTrue = restrict e x True
                            firstTree = buildBDD' firstFalse (2*n) xs
                            secondTree = buildBDD' firstTrue (2*n+1) xs in
                                (n, (n, (x, fst firstTree, fst secondTree)):snd firstTree++snd secondTree)

-- Задача 6 -----------------------------------------
-- Передумова: Кожна змінна (буква) в булевому виразі (BExp) з"являється 
--    точно один раз в списку змінних (Char); немає інших елементів
existsSame :: BDD -> BDDNode -> [(BDDNode, Int)]
existsSame (_, nodes) (a, (b, c, d)) = [ (node, a) | node@(e, (f, g, h))<-nodes, a/=e, b==f, c==g, d==h]

existsSameAll :: BDD -> [(BDDNode, Int)]
existsSameAll bdd@(_, nodes) = concat [ existsSame bdd node | node<-nodes]

sameEnd :: BDD -> [(BDDNode, Int)]
sameEnd (_, nodes) = [ (node, c) | node@(_, (_, c, d))<-nodes, c==d]

optimizeHelper :: BDD -> (BDDNode, Int) -> BDD
optimizeHelper (initial, nodes) (noda@(a, _), with) =  (initial, [ if g == a
                                                                then (e, (f, with, h))
                                                                else if h == a
                                                                    then (e, (f, g, with))
                                                                    else node
                                                                | node@(e, (f, g, h))<-nodes, node/=noda])

optimizeHelperList :: BDD -> [(BDDNode, Int)] -> BDD
optimizeHelperList bdd lst = foldl optimizeHelper bdd lst

optimize :: BDD -> BDD
optimize bdd = if not (null allSame)
                then optimize (optimizeHelper optimized (head allSame))
                else optimized
                where   toreplace = sameEnd bdd
                        optimized = optimizeHelperList bdd toreplace
                        allSame = existsSameAll optimized

buildROBDD :: BExp -> [Char] -> BDD
buildROBDD expr chars = optimize (buildBDD expr chars)

-- Задача 7 -----------------------------------------
boolValue :: Parser BExp
boolValue = (char 'T' >> return (Bvalue True))
        <|> (char 'F' >> return (Bvalue False))

variable :: Parser BExp
variable = Bvar <$> lower

notExpr :: Parser BExp
notExpr = Not <$> (char '!' >> factor)

factor :: Parser BExp
factor = boolValue <|> variable <|> notExpr <|> between (char '(') (char ')') expr

andExpr :: Parser BExp
andExpr = chainl1 factor (char '&' >> return And)

orExp :: Parser BExp
orExp = do
    e1 <- factor
    _ <- char '|'
    Or e1 <$> factor


expr :: Parser BExp
expr = orExpr

fullBexp :: String -> Maybe BExp
fullBexp s = case parse (expr <* eof) "" s of
    Left _  -> Nothing
    Right e -> Just e


------------------------------------------------------
-- Приклади для тестування..
bs1, bs2, bs3, bs4, bs5, bs6, bs7, bs8, bs9 :: String
bs1 = "F"
bs2 = "!(x&(F|y))"
bs3 = "u&T"
bs4 = "d&(x|!y)"
bs5 = "!(d&(x|!y))"
bs6 = "u&x|y&z"
bs7 = "!y|(x|!e)"
bs8 = "u|!u"
bs9 = "z&(y|!y&x)"

b1, b2, b3, b4, b5, b6, b7, b8, b9 :: BExp
b1 = Bvalue False
b2 = Not (And (Bvar 'x') (Or (Bvalue False) (Bvar 'y')))
b3 = And (Bvar 'u') (Bvalue True)
b4 = And (Bvar 'd') (Or (Bvar 'x') (Not (Bvar 'y')))
b5 = Not (And (Bvar 'd') (Or (Bvar 'x') (Not (Bvar 'y'))))
b6 = Or (And (Bvar 'u') (Bvar 'x')) (And (Bvar 'y') (Bvar 'z'))
b7 = Or (Not (Bvar 'y')) (Or (Bvar 'x') (Not (Bvar 'e')))
b8 = Or (Bvar 'u') (Not (Bvar 'u'))
b9 = And (Bvar 'z') (Or (Bvar 'y') (And (Not (Bvar 'y')) (Bvar 'x')))

bdd1, bdd2, bdd3, bdd4, bdd5, bdd6, bdd7, bdd8, bdd9 :: BDD
bdd1 = (0,[])
bdd2 = (2,[(2,('x',4,5)),(4,('y',1,1)),(5,('y',1,0))])
bdd3 = (5,[(5,('u',0,1))])
bdd4 = (2,[(2,('x',4,5)),(4,('y',8,9)),(8,('d',0,1)),(9,('d',0,0)),
           (5,('y',10,11)),(10,('d',0,1)),(11,('d',0,1))])
bdd5 = (3,[(4,('y',8,9)),(3,('x',4,5)),(8,('d',1,0)),(9,('d',1,1)),
           (5,('y',10,11)),(10,('d',1,0)),(11,('d',1,0))])
bdd6 = (2,[(2,('u',4,5)),(4,('x',8,9)),(8,('y',16,17)),(16,('z',0,0)),
           (17,('z',0,1)),(9,('y',18,19)),(18,('z',0,0)),(19,('z',0,1)),
           (5,('x',10,11)),(10,('y',20,21)),(20,('z',0,0)),(21,('z',0,1)),
           (11,('y',22,23)),(22,('z',1,1)),(23,('z',1,1))])
bdd7 = (6,[(6,('x',4,5)),(4,('y',8,9)),(8,('e',1,1)),(9,('e',1,0)),
           (5,('y',10,11)),(10,('e',1,1)),(11,('e',1,1))])
bdd8 = (2,[(2,('u',1,1))])
bdd9 = (2,[(2,('x',4,5)),(4,('y',8,9)),(8,('z',0,0)),(9,('z',0,1)),(5,('y',10,11)),(10,('z',0,1)),(11,('z',0,1))])



