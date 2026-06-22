{-# OPTIONS_GHC -Wall #-}
module RomaniukA08 where

data BinTreeM a = EmptyM
                | NodeM a Int (BinTreeM a) (BinTreeM a)
                   deriving (Show, Eq)
data Tree23 a  = Leaf a
               | Node2 (Tree23 a) a (Tree23 a)
               | Node3 (Tree23 a) a (Tree23 a) a (Tree23 a)
               | Empty23     -- порожнє 2-3-дерево!!!
                   deriving (Eq, Show)

-- Задача 1 ------------------------------------
getValue :: (Ord a) => BinTreeM a -> a
getValue EmptyM = undefined
getValue (NodeM b _ _ _) = b

isSearch :: (Ord a) => BinTreeM a -> Bool
isSearch EmptyM = True
isSearch (NodeM v k b c) = k > 0
                            && case b of
                                EmptyM -> True
                                _ -> isSearch b && getValue b < v
                            && case c of
                                EmptyM -> True
                                _ -> isSearch c && getValue c > v

-- Задача 2 ------------------------------------
elemSearch :: (Ord a) => BinTreeM a -> a -> Bool
elemSearch EmptyM _ = False
elemSearch (NodeM v _ lt rt) val = val == v
                                || if val > v then elemSearch rt val
                                    else elemSearch lt val

-- Задача 3 ------------------------------------
insSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a
insSearch EmptyM b = NodeM b 1 EmptyM EmptyM
insSearch (NodeM v k lt rt) val | val == v = NodeM v (k+1) lt rt
                                | val > v = NodeM v k lt (insSearch rt val)
                                | otherwise = NodeM v k (insSearch lt val) rt

-- Задача 4 ------------------------------------
getLeftMostNode :: (Ord a) => BinTreeM a -> (a, Int)
getLeftMostNode tree = case tree of
                        EmptyM -> undefined
                        NodeM v k lt _ -> if lt == EmptyM then (v, k)
                                            else getLeftMostNode lt

getWithouthLeftMostNode :: (Ord a) => BinTreeM a -> BinTreeM a
getWithouthLeftMostNode tree = case tree of
                                EmptyM -> EmptyM
                                NodeM v k lt rt -> if lt == EmptyM then EmptyM
                                                    else NodeM v k (getWithouthLeftMostNode lt) rt

delSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a
delSearch EmptyM _ = EmptyM
delSearch (NodeM v k lt rt) val | val == v = if k > 1   then NodeM v (k-1) lt rt
                                                        else if lt == EmptyM then rt 
                                                            else let got = getLeftMostNode rt in
                                                            NodeM (fst got) (snd got) lt (getWithouthLeftMostNode rt)
                                | val > v = NodeM v k lt (delSearch rt val)
                                | otherwise = NodeM v k (delSearch lt val) rt


-- Задача 5 ------------------------------------
findMin :: (Ord a) => BinTreeM a -> (a, BinTreeM a)
findMin EmptyM = undefined
findMin (NodeM v k lt rt) = if lt == EmptyM then
                                if k>1 then (v, NodeM v (k-1) lt rt)
                                else (v, rt)
                            else (fnd1, NodeM v k fnd2 rt)
                    where (fnd1, fnd2) = findMin lt

sortListHelper :: (Ord a) => BinTreeM a -> [a]
sortListHelper EmptyM = []
sortListHelper tree = minVal : sortListHelper remainTree
            where   (minVal, remainTree) = findMin tree

sortList :: (Ord a) => [a] -> [a]
sortList lst = sortListHelper tree
        where   tree = foldl insSearch EmptyM lst

-- Задача 6-----------------------------------------
getLeftMostNode23 :: (Ord a) => Tree23 a -> a
getLeftMostNode23 tree = case tree of 
                        Empty23 -> undefined
                        Leaf a -> a
                        Node2 a _ _ -> getLeftMostNode23 a 
                        Node3 a _ _ _ _ -> getLeftMostNode23 a

headMoreThanNode :: (Ord a) => a -> Tree23 a -> Bool
headMoreThanNode val (Node2 _ c _)  = val >= c
headMoreThanNode val (Node3 _ c _ e _) = val >= c && val >= e
headMoreThanNode _ Empty23 = True
headMoreThanNode c (Leaf b) = c >= b

headLessThanNode :: (Ord a) => a -> Tree23 a -> Bool
headLessThanNode val (Node2 _ c _)  = val <= c
headLessThanNode val (Node3 _ c _ e _) = val <= c && val <= e
headLessThanNode _ Empty23 = False
headLessThanNode c (Leaf b) = c <= b

isTree23  :: (Ord a) => Tree23 a -> Bool
isTree23 tr = case tr of
                Empty23 -> True
                Leaf _ -> True
                Node2 b c d -> isTree23 b && isTree23 d
                                        && c `headMoreThanNode` b
                                        && c `headLessThanNode` d
                                        && c == getLeftMostNode23 d
                Node3 b c d e f -> isTree23 b && isTree23 d && isTree23 f
                                        && c `headMoreThanNode` b
                                        && c `headLessThanNode` d
                                        && e `headMoreThanNode` d
                                        && e `headLessThanNode` f
                                        && c == getLeftMostNode23 d
                                        && e == getLeftMostNode23 f

-- Задача 7-----------------------------------------
elemTree23 :: (Ord a) => Tree23 a -> a -> Bool
elemTree23 tr val = case tr of
                        Empty23 -> False
                        Leaf b -> b==val
                        Node2 b _ d -> elemTree23 b val || elemTree23 d val
                        Node3 b _ d _ f -> elemTree23 b val || elemTree23 d val || elemTree23 f val

-- Задача 8-----------------------------------------
findMin23 :: (Ord a) => Tree23 a -> a
findMin23 tree = case tree of
                Leaf a -> a
                Empty23 -> undefined
                Node2 b _ d -> if b == Empty23
                                then findMin23 d
                                else findMin23 b
                Node3 b _ d _ f -> if b == Empty23
                                    then if d == Empty23
                                        then findMin23 f
                                        else findMin23 d
                                    else findMin23 b

deleteValue23 :: (Ord a) => Tree23 a -> a -> Tree23 a
deleteValue23 tree val = case tree of
                Leaf a -> if a == val then Empty23 else Leaf a
                Empty23 -> Empty23
                Node2 b c d -> if val < c
                                then Node2 (deleteValue23 b val) c d
                                else if val > c then let deletedNode = deleteValue23 d val in
                                    if deletedNode == Empty23 && b == Empty23 then Empty23
                                    else Node2 b c deletedNode
                                else let    deletedNode1 = deleteValue23 b val
                                            deletedNode2 = deleteValue23 d val in
                                        if deletedNode1 == Empty23 && deletedNode2 == Empty23 then Empty23
                                            else Node2 deletedNode1 c deletedNode2
                Node3 b c d e f -> if val < c
                                    then Node3 (deleteValue23 b val) c d e f
                                    else if val < e
                                        then Node3 b c (deleteValue23 d val) e f
                                        else let deletedNode = deleteValue23 f val in
                                            if deletedNode == Empty23 && b == Empty23 && d == Empty23 then Empty23
                                            else Node3 b c d e (deleteValue23 f val)

deleteMin23 :: (Ord a) => Tree23 a -> Tree23 a
deleteMin23 tree = deleteValue23 tree (findMin23 tree)

listFromTree23 :: (Ord a) => Tree23 a -> [a]
listFromTree23 Empty23 = []
listFromTree23 tree = minVal : listFromTree23 newTree
                where   minVal = findMin23 tree
                        newTree = deleteMin23 tree

eqTree23 :: (Ord a) => Tree23 a -> Tree23 a -> Bool
eqTree23 tree1 tree2 = listFromTree23 tree1 == listFromTree23 tree2

-- Задача 9-----------------------------------------
insTree23 :: (Ord a) => Tree23 a -> a -> Tree23 a
insTree23 tree val = case insert val tree of
                        (a, Nothing) -> a
                        (a, Just (w, b)) -> Node2 a w b

-- isTerminal tr = True <=> якщо сини вузла tr - листки !!
isTerminal :: (Ord a) => Tree23 a -> Bool
isTerminal (Node2 (Leaf _) _ _)     = True
isTerminal (Node3 (Leaf _) _ _ _ _) = True
isTerminal _                        = False

-- Результат вставки вузла в 2-3-дерево, 
--   корінь якого - вузол вида Node2 або Node3 є об`єкт із (Tree23 a, Maybe (a, Tree23 a))
--   : (a, Nothing) - результат вставки - одне 2-3-дерево a 
--   : (a, Just (w, b)) - результат вставки два 2-3-дерева a i b (w - найменше значення в b)
--  insert v tr - додає значення v в довільне дерево tr
insert :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insert v tr | isTerminal tr = insTerm v tr
            | otherwise     = insNode v tr

-- insTerm v tr - додається значення v в дерево tr з конем - термінальний вузол 
getValue23 :: Tree23 a -> a
getValue23 (Leaf b) = b
getValue23 _ = undefined

insTerm :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insTerm v tree = case tree of
                    Node2 l x xV -> if v < getValue23 l then (Node3 (Leaf v) (getValue23 l) l x xV, Nothing)
                                    else if v < x then (Node3 l (getValue23 l) (Leaf v) x xV, Nothing)
                                    else (Node3 l (getValue23 l) xV x (Leaf v), Nothing)
                    Node3 l x xV y yV -> if v < getValue23 l then (Node2 (Leaf v) (getValue23 l) l, Just (x, (Node2 xV y yV)))
                                    else if v < x then (Node2 l (getValue23 l) (Leaf v), Just (x, (Node2 xV y yV)))
                                    else if v < y then (Node2 l x xV, Just (v, (Node2 (Leaf v) y yV)))
                                    else (Node2 l (getValue23 l) xV, Just (y, (Node2 yV y (Leaf v))))
                    _ -> undefined

-- insNode v tr - додає значення v в дерево tr з корнем - нетермінальний вузол  
insNode :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insNode v tree = case tree of
                    Empty23 -> (Leaf v, Nothing)
                    Leaf b -> if v < b then (Node2 (Leaf v) b (Leaf b), Nothing)
                                else (Node2 (Leaf b) b (Leaf v), Nothing)
                    Node2 l x tr -> if v < x
                                        then let (a, Just (w, b)) = insert v l in
                                            (Node3 a w b x tr, Nothing)
                                        else let (a, Just (w, b)) = insert v tr in
                                            (Node3 l x a w b, Nothing)
                    Node3 tl x tm y tr -> if v < x
                                            then let (a, Just (w, b)) = insert v tl in
                                                (Node2 a w b, Just (x, Node2 tm y tr))
                                        else if v < y
                                            then let (a, Just (w, b)) = insert v tm in
                                                (Node2 tl x a, Just (w, Node2 b y tr))
                                        else
                                            let (a, Just (w, b)) = insert v tr in
                                                (Node2 tl x tm, Just (y, Node2 a w b))



---  Бінарні дерева 
bm :: BinTreeM Char
bm = NodeM  't' 2
            (NodeM 'a' 1  EmptyM
                    (NodeM 'e' 1
                             (NodeM 'd' 2 EmptyM EmptyM)
                             (NodeM 'f' 1 EmptyM EmptyM)
                    )
            )
            (NodeM 'w' 2  EmptyM EmptyM)

---- 2-3-дерева
tr1, tr2, tr3, tr4,tr5 :: Tree23 Int
tr1 =  Node2 (Node2 (Node2 (Leaf 0) 1 (Leaf 1))
                     2
                    (Node2 (Leaf 2) 3 (Leaf 3)))
              4
             (Node2 (Node2 (Leaf 4) 5 (Leaf 5))
                     6
                    (Node2 (Leaf 6) 7 (Leaf 7)))
tr2 =  Node3 (Node2 (Leaf 0) 1 (Leaf 1))
              2
             (Node3 (Leaf 2) 3 (Leaf 3) 4 (Leaf 4))
              5
             (Node3 (Leaf 5) 6 (Leaf 6) 7 (Leaf 7))

tr3 = Node3 (Node2 (Leaf 2) 5 (Leaf 5))
            7
            (Node3 (Leaf 7) 8 (Leaf 8) 12 (Leaf 12))
            16
            (Node2 (Leaf 16) 19 (Leaf 19))

tr4 = Node3 (Node2 (Leaf 2) 5 (Leaf 5))
            7
            (Node3 (Leaf 7) 8 (Leaf 8) 12 (Leaf 12))
            16
            (Node3 (Leaf 16) 18 (Leaf 18) 19 (Leaf 19))

tr5 = Node2 (Node2 (Node2 (Leaf 2) 5 (Leaf 5))
                    7
                   (Node2 (Leaf 7) 8 (Leaf 8))
            )
            10
            (Node2 (Node2 (Leaf 10) 12 (Leaf 12))
                   16
                   (Node3 (Leaf 16) 18 (Leaf 18) 19 (Leaf 19))
            )

