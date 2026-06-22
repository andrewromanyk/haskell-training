{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use isNothing" #-}
module RomaniukA12 where

--  Пакет parsec може бути закритим (hidden), 
--  щоб відкрити його потрібно завантажити файл з опцією -package parsec
--  ghci xxxx.hs -package parsec // ghc xxxx.hs -package parsec 

import Text.Parsec.String
import Text.Parsec   --- parse

data Recur = Zero | Succ | Sel Int Int
           | Super Recur [Recur]
           | Prim Recur Recur
           | Mini Recur Int
           | Name String  deriving (Show, Eq)
type System = [(String,Recur)]

-- Задача 1 ------------------------------------
findRecur :: System -> String -> Recur
findRecur [] _ = error "empty list"
findRecur ((a,b):sys) name  | name == a = b
                            | otherwise = findRecur sys name

isNumbConst :: System -> Recur -> Bool
isNumbConst _ Zero = True
isNumbConst sys (Super Succ [a]) = isNumbConst sys a
isNumbConst sys (Name name) = isNumbConst sys (sys `findRecur` name)
isNumbConst _ _ = False

-- Задача 2 ------------------------------------
evRank :: System -> Recur -> Int
evRank _ Zero = 1
evRank _ Succ = 1
evRank _ (Sel n _) = n
evRank sys (Super _ al) = evRank sys (head al)
evRank sys (Prim _ st) = evRank sys st - 1
evRank sys (Mini b _) = evRank sys b - 1
evRank sys (Name name) = evRank sys (sys `findRecur` name)

-- Задача 3 ------------------------------------
noDuplicates :: [String] -> Bool
noDuplicates [] = True
noDuplicates (x:xs) = and [ a/=x | a<-xs] && noDuplicates xs

funcOnlyHasNames :: Recur -> [String] -> Bool
funcOnlyHasNames (Super a b) names = funcOnlyHasNames a names
                    && and [ funcOnlyHasNames func names| func<-b]
funcOnlyHasNames (Prim a b) names = funcOnlyHasNames a names && funcOnlyHasNames b names
funcOnlyHasNames (Mini a _) names = funcOnlyHasNames a names
funcOnlyHasNames (Name a) names = a `elem` names
funcOnlyHasNames _ _ = True

isNames :: System -> Bool
isNames sys = noDuplicates names && and [ funcOnlyHasNames a b | (a, b)<-funcsAndNamesBefore]
                where   names = [a | (a, _)<- sys]
                        funcsAndNamesBefore = [ (sys `findRecur` name, reverse . tail $ dropWhile (/=name) (reverse names))  | name<-names]

-- Задача 4 ------------------------------------
isRecur :: System -> Recur -> Bool
isRecur sys (Super a b) = isRecur sys a && and [isRecur sys func | func<-b]
isRecur sys (Prim a b) = isRecur sys a && isRecur sys b
isRecur sys (Mini a _) = isRecur sys a
isRecur sys (Name name) = name `elem` names
                where names = [a | (a, _)<-sys]
isRecur _ _ = True

-- Задача 5 ------------------------------------
eval :: System -> Recur -> [Int] -> Int
eval _ Zero [_] = 0
eval _ Succ [a] = a+1
eval sys recur@(Sel _ b) vals = if evRank sys recur == length vals && b <= length vals
                                    then vals !! (b-1)
                                    else error "values list does not align with recursive function"
eval sys (Super a b) vals = eval sys a [eval sys func vals | func<-b]

eval sys (Prim a b) vals
  | last vals == 0 = if evRank sys a /= length vals-1
                                    then error "values list does not align with recursive function"
                                    else eval sys a (init vals)
  | evRank sys b /= length vals+1 = error "values list does not align with recursive function"
  | otherwise = eval sys b (init vals++[last vals - 1]++[eval sys (Prim a b) (init vals++[last vals - 1])])

-- eval sys (Mini a b) vals = if evRank sys a - 1 /= length vals || b<0
--                                     then error "values list does not align with recursive function"
--                                     else  takeFirstZero [ eval sys a (vals++[y]) | y<-[0..b]]
eval sys (Name name) vals = eval sys (sys `findRecur` name) vals
eval _ _ _ = error "wrong definition of primitive recurrent function"

-- Задача 6 ------------------------------------
takeFirstZeroHelper :: [Maybe Int] -> Int -> Maybe Int
takeFirstZeroHelper [] _ = Nothing
takeFirstZeroHelper (x:xs) ind = case x of
                        Just a -> if a == 0
                                    then Just ind
                                    else takeFirstZeroHelper xs (ind+1)
                        Nothing -> Nothing

takeFirstZero :: [Maybe Int] -> Maybe Int
takeFirstZero a = takeFirstZeroHelper a 0

evalPart :: System -> Recur -> [Int] -> Maybe Int
evalPart _ Zero [_] = Just 0
evalPart _ Succ [a] = Just (a+1)
evalPart sys recur@(Sel _ b) vals = if evRank sys recur == length vals && b <= length vals
                                    then Just (vals !! (b-1))
                                    else error "values list does not align with recursive function1"
evalPart sys (Super a b) vals = if or [evals == Nothing| evals<-evaluated] then Nothing
                                    else evalPart sys a [case func of
                                                            Just val -> val
                                                            Nothing -> error "function not evaluable" | func<-evaluated]
                where evaluated = [evalPart sys func vals | func<-b]
evalPart sys (Prim a b) vals
  | last vals == 0 = if evRank sys a /= length vals-1
                                    then if isNumbConst sys a
                                            then evalPart sys a vals
                                            else error "values list does not align with recursive function2"
                                    else evalPart sys a (init vals)
  | evRank sys b /= length vals+1 = error "values list does not align with recursive function3"
  | otherwise = case evaluated of
                    Just val -> evalPart sys b (init vals++[last vals - 1]++[val])
                    Nothing -> Nothing
        where evaluated = evalPart sys (Prim a b) (init vals++[last vals - 1])

evalPart sys (Mini a b) vals = if evRank sys a - 1 /= length vals || b<0
                                    then error "values list does not align with recursive function4"
                                    else  takeFirstZero [ evalPart sys a (vals++[y]) | y<-[0..b]]
evalPart sys (Name name) vals = evalPart sys (sys `findRecur` name) vals
evalPart _ _ _ = error "wrong definition of primitive recurrent function"

-- Задача 7 ------------------------------------
digitChars :: String
digitChars = "0123456789"

digitCharToInt :: Char -> Int
digitCharToInt ch = head [ind | ind<-[0..length digitChars-1], digitChars!!ind==ch]

funcName :: Parser String
funcName = do
    firstChar <- letter
    rest <- many alphaNum
    return (firstChar : rest)

stringToInt :: String -> Int
stringToInt nums = sum [ digitCharToInt (nums!!ind) * (10^(length nums-ind-1)) | ind <- [0..length nums-1]]

number :: Parser Int
number = do
    nums <- many digit
    return (stringToInt nums)

a1 :: Parser Recur
a1 = do
    _ <- string "a1"
    return Succ

z1 :: Parser Recur
z1 = do
    _ <- string "z1"
    return Zero

s__ :: Parser Recur
s__ = do
    _ <- char 's'
    a <- digit
    Sel (digitCharToInt a) . digitCharToInt <$> digit

iden :: Parser Recur
iden = Name <$> funcName

base :: Parser Recur
base = try a1 <|> try z1 <|> try s__ <|> iden

nameFuncsAfterComma :: Parser Recur
nameFuncsAfterComma = do
                _ <- char ','
                _ <- spaces
                a <- funcExpr
                _ <- spaces
                return a

nameFuncs :: Parser [Recur]
nameFuncs = do
    a <- funcExpr
    _ <- spaces
    b <- many nameFuncsAfterComma
    return (a:b)

super :: Parser Recur
super = do
    _ <- spaces
    a <- funcExpr
    _ <- spaces
    _ <- char ':'
    _ <- spaces
    b <- nameFuncs
    _ <- spaces
    return (Super a b)


prim :: Parser Recur
prim = do
    _ <- spaces
    a <- funcExpr
    _ <- spaces
    _ <- char ','
    _ <- spaces
    Prim a <$> funcExpr

mini :: Parser Recur
mini = do
    _ <- spaces
    a <- funcExpr
    _ <- spaces
    _ <- char ','
    _ <- spaces
    b <- number 
    _ <- spaces
    return (Mini a b)

funcExpr :: Parser Recur
funcExpr = base
    <|> between (char '(') (char ')') super
    <|> between (char '[') (char ']') prim
    <|> between (char '{') (char '}') mini

fullExpr :: Parser (String, Recur)
fullExpr = do
        _ <- spaces
        a <- funcName
        _ <- spaces
        _ <- char '='
        _ <- spaces
        b <- funcExpr
        _ <- spaces
        _ <- char ';'
        _ <- spaces
        return (a, b)

manyFullExpr :: Parser [(String, Recur)]
manyFullExpr = many fullExpr

parseRec :: String -> Maybe System
parseRec "" = Nothing
parseRec s = case parse (manyFullExpr <* eof) "" s of
    Left _  -> Nothing
    Right e -> Just e

---------------------Тестові дані -  -------
syst1, syst2 :: System
syst1 = [("const0", Zero)
   , ("const0v2", Super Zero [Sel 2 1])
   , ("const0v3", Super Zero [Sel 3 1])
   , ("const1v2", Super Succ [Super Zero [Sel 2 1]])
   , ("const2", Super Succ [Super Succ [Zero]])
   , ("addition", Prim (Sel 1 1) (Super Succ [Sel 3 3 ]))
   , ("multiplication", Prim Zero (Super (Name "addition") [Sel 3 3, Sel 3 1]))
   , ("notSignum", Prim (Super Succ [Zero]) (Super Zero [Sel 2 1]))
   , ("subtract1", Prim Zero (Sel 2 1))
   , ("subtraction", Prim (Sel 1 1) (Super (Name "subtract1") [Sel 3 3]))
   , ("subtractionRev", Super (Name "subtraction") [Sel 2 2, Sel 2 1])
   , ("subtractionAbs", Super (Name "addition") [Name "subtraction", Name "subtractionRev"])
   , ("subtractionAbs3", Super (Name "subtractionAbs") [Sel 3 1, Super (Name "addition") [Sel 3 2, Sel 3 3]])
   , ("subtractionPart", Mini (Name "subtractionAbs3") 100)
   ]

syst2 = [("f1", Super Succ [Zero])
        ,("f2", Super Succ [Name "f2"])
        ]


sysStr1,sysStr2 :: String
sysStr1 = " const0 = z1; const0v2  = (z1 : s21); const0v3 = (z1:s31);\n\
          \  const1v2 = (a1 : (z1 : s21));  \n\
          \  const2= (a1:(a1:z1)); addition = [s11, (a1:s33)] ;\n\
          \  multiplication = [z1 , (addition: s33,s31)]; \n\
	  \  notSignum = [(a1:z1),(z1:s21)];\n\
	  \  subtract1 = [z1,s21]; subtraction = [s11, (subtract1:s33)];\n\
	  \  subtractionRev = (subtraction : s22, s21);\n\
          \  subtractionAbs = (addition: subtraction, subtractionRev); \n\
          \  subtractionAbs3=(subtractionAbs:s31, (addition:s32,s33))  ;\n \
          \ subtractionPart = {subtractionAbs3, 100 };"

sysStr2 = " f1 = (a1:z1); f2 = (a1, f2);"
