{-# OPTIONS_GHC -Wall #-}
module RomaniukA00 where

type Algorithm    = [Substitution]
type Substitution = (String,String,Bool)
type ConfigA      = (Bool, Int, String)

data Rose a = Node a (Forest a) deriving  (Show, Eq)
type Forest a = [Rose a]

-- Задача 1 -----------------------------------------
deleteChar :: String -> Char -> String
deleteChar [] _ = []
deleteChar (x:xs) ch = if x == ch then xs
                        else x:deleteChar xs ch

bagSubbag :: String -> String -> Bool
bagSubbag [] [] = True
bagSubbag _ [] = False
bagSubbag [] _ = True
bagSubbag xs (a:b) =  bagSubbag (xs `deleteChar` a) b

-- Задача 2 -----------------------------------------
findFirstSame :: String -> String -> String
findFirstSame x y = [ a | a<-x, b<-y, a==b]

bagUnionHelper :: String -> String -> String -> String
bagUnionHelper [] y hlp = y++hlp
bagUnionHelper x [] hlp = x++hlp
bagUnionHelper x y hlp = let sames = findFirstSame x y in
                            if null sames then x++y++hlp
                            else bagUnionHelper (deleteChar x (head sames)) (deleteChar y (head sames)) (head sames:hlp)

bagUnion :: String -> String -> String
bagUnion x y = bagUnionHelper x y []


-- Задача  3 -----------------------------------------
bagIntersectHelper :: String -> String -> String -> String
bagIntersectHelper x y hlp = let sames = findFirstSame x y in
                            if null sames then hlp
                            else bagIntersectHelper (deleteChar x (head sames)) (deleteChar y (head sames)) (head sames:hlp)

bagIntersect :: String -> String -> String
bagIntersect x y = bagIntersectHelper x y []

--- Задача 4 ----------------------------------------
digitChars :: String
digitChars = "0123456789"

digitCharToInt :: Char -> Int
digitCharToInt ch = head [ind | ind<-[0..length digitChars-1], digitChars!!ind==ch]

stringToInt :: String -> Int
stringToInt nums = sum [ digitCharToInt (nums!!ind) * (10^(length nums-ind-1)) | ind <- [0..length nums-1]]

intToChar :: Int -> Char
intToChar a = digitChars !! a

intToString :: Int -> String
intToString 0 = ""
intToString num =  intToString (num `div` 10) ++ [intToChar (num `mod` 10)]


data Op = Add | Sub | Mul
data Expr = Val Int | App Op Expr Expr
type Result = (Expr,Int)
instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"

instance Show Expr where
  show (Val n) = show n
  show (App op e1 e2) =
              show e1 ++ show op ++ show e2

valid1 :: Op -> Int -> Int -> Bool
valid1 Add x y = x<=y
valid1 Sub x y = x>y
valid1 Mul x y = (x/=1) && (y/=1) && (x<=y)

apply :: Op -> Int -> Int -> Int
apply Add v1 v2 = v1 + v2
apply Sub v1 v2 = v1 - v2
apply Mul v1 v2 = v1 * v2

splits  :: [a] ->[([a],[a])]
splits xs = [ (init xs, [last xs]) ]

combine1 :: Result -> Result -> [Result]
combine1 (l,x) (r,y) =
       [(App o l r, apply o x y) | o <- [Add,Sub,Mul]]

results :: [Int] -> [Result]
results []  = []
results [n] = [(Val n,n) | n>0]
results ns  = [e | (ls,rs) <- splits ns,
                          l <- results ls,
                          r <- results rs,
                          e <- combine1 l r ]


solutions1 :: Int -> [Int] -> [Expr]
solutions1 n ns = [e | (e,m) <- results ns, m == n]

genExpr :: Int -> Int -> [String]
genExpr a goal = map show (solutions1 goal (map digitCharToInt (intToString a)))


--- Задача 5 ----------------------------------------
data Op2 = Add2 | Sub2 | Mul2
data Expr2 = Val2 Int | App2 Op2 Expr2 Expr2
type Result2 = (Expr2,Int)
instance Show Op2 where
  show Add2 = "+"
  show Sub2 = "-"
  show Mul2 = "*"

instance Show Expr2 where
  show (Val2 n) = show n
  show (App2 op e1 e2) =
            "(" ++ show e1 ++ show op ++ show e2 ++ ")"

valid12 :: Op2 -> Int -> Int -> Bool
valid12 Add2 x y = x<=y
valid12 Sub2 x y = x>y
valid12 Mul2 x y = (x/=1) && (y/=1) && (x<=y)

apply2 :: Op2 -> Int -> Int -> Int
apply2 Add2 v1 v2 = v1 + v2
apply2 Sub2 v1 v2 = v1 - v2
apply2 Mul2 v1 v2 = v1 * v2

splits2  :: [a] ->[([a],[a])]
splits2 xs = [ splitAt i xs | i<-[1..length xs-1] ]

combine12 :: Result2 -> Result2 -> [Result2]
combine12 (l,x) (r,y) =
       [(App2 o l r, apply2 o x y) | o <- [Add2,Sub2,Mul2]]

results2 :: [Int] -> [Result2]
results2 []  = []
results2 [n] = [(Val2 n,n) | n>0]
results2 ns  = [e | (ls,rs) <- splits2 ns,
                          l <- results2 ls,
                          r <- results2 rs,
                          e <- combine12 l r ]


solutions12 :: Int -> [Int] -> [Expr2]
solutions12 n ns = [e | (e,m) <- results2 ns, m == n]


genExprBracket :: Int -> Int -> [String]
genExprBracket a goal = map show (solutions12 goal (map digitCharToInt (intToString a)))

-- Задача  6 -----------------------------------------
leftIsSame :: String -> String -> Bool
leftIsSame [] _ = True
leftIsSame _ [] = False
leftIsSame (x:l) (y:str) = x == y && leftIsSame l str

subst :: Substitution -> String -> String
subst rule@(l, r, _) str = if leftIsSame l str then r ++ drop (length l) str
                        else head str : subst rule (tail str)

substitute :: Substitution -> Int -> String -> String
substitute (l, r, _) ind str = take ind str ++ r ++ drop (ind + length l) str

-- Задача 7 -----------------------------------------
findPosition :: String -> Substitution -> [(Substitution,Int)]
findPosition str rule@(l, _, _) = [ (rule, ind) | ind <- [0..length str - length l], leftIsSame l (drop ind str)]

-- Задача 8 -----------------------------------------
findAll :: Algorithm -> String -> [(Substitution,Int)]
findAll algo str = concat [findPosition str rule| rule<-algo]

--- Задача 9 ----------------------------------------
stepA :: Algorithm -> ConfigA -> ConfigA
stepA _ conf@(False, _, _) = conf
stepA algo (_, i, curr) = let (rule@(_, _, cont), ind) = head (findAll algo curr) in
                (not cont, i+1, substitute rule ind curr)

-- Задача 10 ------------------------------------
evalA :: Algorithm -> Int -> String -> Maybe String
evalA algo i curr = case stepA algo (True, 0, curr) of
                        (False, _, res) -> Just res
                        (True, _, res) -> if i == 1 then Nothing
                                            else evalA algo (i-1) res

-- Задача 11 -----------------------------------------	
-- data Rose a = Node a (Forest a) 
-- deriving  (Show, Eq)
-- type Forest a = [Rose a]

rank :: Rose a -> Int
rank (Node _ xs) = length xs

-- Задача 12-----------------------------------------
isBinomTree :: Ord a => Rose a -> Bool
isBinomTree (Node val xs) = and [ let (Node val2 _) = xs!!(x-1) in
    rank (xs!!(x-1)) == (length xs - x) && val <= val2 && isBinomTree (xs!!(x-1))
    | x<-[1..length xs]]

-- Задача 13 -----------------------------------------
isBinomHeap :: Ord a => Forest a -> Bool
isBinomHeap [] = True
isBinomHeap [a] = isBinomTree a
isBinomHeap (x:y:xs) =  isBinomTree x 
                        && rank x < rank y 
                        && isBinomHeap (y:xs)

-- Задача 14 -----------------------------------------
combineTrees :: Ord a => Rose a -> Rose a -> Rose a
combineTrees x@(Node a xs) y@(Node b ys) = if a <=b then Node a (y:xs)
                                                else Node b (x:ys)

-- Задача 15 -----------------------------------------
extractMin :: Ord a => Forest a -> a
extractMin forest = minimum [ x | (Node x _) <- forest]

-- Задача 16-----------------------------------------
mergeHeaps :: Ord a => Forest a -> Forest a -> Forest a
mergeHeaps x [] = x
mergeHeaps [] x = x
mergeHeaps hp1f@(x:hp1) hp2f@(y:hp2)
        | rank x == rank y = let combined = combineTrees x y in
                                if null hp2 
                                then combined : hp1
                                else 
                                    if rank combined < rank (head hp2) 
                                    then mergeHeaps hp1 (combined:hp2)
                                    else mergeHeaps (combined:hp1) hp2
        | rank x < rank y = x : mergeHeaps hp1 hp2f
        | otherwise = y : mergeHeaps hp1f hp2

-- Задача 17-----------------------------------------
insert :: Ord a => a -> Forest a -> Forest a
insert a = mergeHeaps [Node a []]

-- Задача 18-----------------------------------------
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

getValue :: Rose a -> a
getValue (Node v _) = v

removeMin :: Ord a => Forest a -> (Rose a, Forest a)
removeMin [] = undefined
removeMin [t] = (t, [])
removeMin (t:ts) = let (minT, rest) = removeMin ts in 
                if getValue t <= getValue minT
                    then (t, ts)
                    else (minT, t : rest)

deleteMin :: Ord a => Forest a -> Forest a
deleteMin forest = let (Node _ toDelete, toLeave) = removeMin forest in
            mergeHeaps toLeave  (reverseList toDelete)

-- Задача 19-----------------------------------------
extractList :: Ord a => Forest a -> [a]
extractList [] = []
extractList forest = extractMin forest : extractList (deleteMin forest)

binomSort :: Ord a => [a] -> [a]
binomSort lst = extractList (foldr insert [] lst)

-- Задача 20 -----------------------------------------
hasRank :: Forest a -> Int -> Bool
hasRank forest rang = or [rank rose == rang| rose<-forest] 

generateList :: Forest a -> Int -> [Int]
generateList _ 0 = [] 
generateList forest size = if hasRank forest (size-1) 
                            then 1:generateList forest (size-1)
                            else 0:generateList forest (size-1)

toBinary :: Forest a -> [Int]
toBinary forest = let listSize = rank (last forest) + 1 in
                    generateList forest listSize



---------------------Тестові дані - нормальні алгоритми Маркова -------
clearBeginOne, addEnd, reversal, multiply:: Algorithm
-- стирає перший символ вхідного слова (алфавіт {a,b})
clearBeginOne = [ ("ca", "", True)
                , ("cb", "", True)
                , ("", "c", False)
                ]

-- дописує abb в кінець вхідного слова (алфавіт {a,b})
addEnd = [ ("ca", "ac", False)
         , ("cb", "bc", False)
         , ("c", "abb", True)
         , ("", "c", False)
         ]
-- зеркальне відображення вхідного слова (алфавіт {a,b})
reversal = [ ("cc", "d", False)
          , ("dc", "d", False)
          , ("da", "ad", False)
          , ("db", "bd", False)
          , ("d", "", True)
          , ("caa", "aca", False)
          , ("cab", "bca", False)
          , ("cba", "acb", False)
          , ("cbb", "bcb", False)
          , ("", "c", False)
          ]

-- добуток натуральних чисел 
--  multiply ("|||#||") = "||||||"  3*2 = 6
multiply = [("a|", "|ba", False)
            ,("a", "", False)
            ,("b|", "|b", False)
            ,("|#", "#a", False)
            ,("#", "c", False)
            ,("c|", "c", False)
            ,("cb", "|c", False)
            ,("c", "", True)
            ]

-----------------------------------------------------  
-- Приклади деяких дерев...

t1, t2, t3, t4, t5, t6, t7, t8 :: Rose Int
--  Зауваження: t7 - результат злиття t5 і t6

-- t1 .. t4 з'являються на Мал. 1...
t1 = Node 4  []
t2 = Node 1 [Node 5 []]
t3 = Node 2 [Node 8 [Node 9 []],
             Node 7 []]
t4 = Node 2 [Node 3 [Node 6 [Node 8 []],
                     Node 10 []],
             Node 8 [Node 9 []],
             Node 7 []]

-- t5 і t6 зліва на Мал.2; t7 - справа на Мал.2
t5 = Node 4 [Node 6 [Node 8 []],
                     Node 10 []]
t6 = Node 2 [Node 8 [Node 9 []], Node 7 []]
t7 = Node 2 [Node 4 [Node 6 [Node 8 []], Node 10 []],
             Node 8 [Node 9 []],
             Node 7 []]

-- Додаткове дерево...
t8 = Node 12 [Node 16 []]

------------------------------------------------------
-- Приклади деяких куп...

h1, h2, h3, h4, h5, h6, h7 :: Forest Int
-- Two arbitrary heaps for testing...
h1 = [t2, t7]
h2 = [Node 1 [Node 12 [Node 16 []],
              Node 5 []],
      Node 2 [Node 4 [Node 6 [Node 8 []],
                      Node 10 []],
              Node 8 [Node 9 []],
              Node 7 []]]

-- h3 показана на Мал.3...
h3 = [t1, t2, t4]

-- Дві додаткові купи використовуються далі. Вони зліва на Мал.4(a)...

h4 = [t2, t5]
h5 = [t1, t8]

-- h6 - результат злиття h4 і h5, справа на Мал.4(b)...
h6 = [Node 4 [],
      Node 1 [Node 4 [Node 6  [Node 8 []],
                      Node 10 []],
              Node 12 [Node 16 []],
              Node 5 []]]

-- h7 показана на Мал.5...
h7 = [Node 4 [Node 4 [Node 12 [Node 16 []],
                      Node 5 []],
              Node 6 [Node 8 []],
              Node 10 []]]