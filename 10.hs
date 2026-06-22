{-# OPTIONS_GHC -Wall #-}
module RomaniukA06 where

import Data.Char(isSpace, isDigit, isLetter)

type Name       = String
type Attributes = [(String, String)]
data XML        =  Text String | Element Name Attributes [XML] deriving (Eq, Show)

-- Задача 1 -----------------------------------------
spaces :: String -> String
spaces [] = []
spaces xss@(s:xs)   | isSpace s = spaces xs
                    | otherwise = xss

-- Задача 2.a ----------------------------------------- 
manyHelper :: String -> (Char -> Bool) -> (String,String)
manyHelper [] _ = ("", "")
manyHelper xss@(x:xs) func  | func x = (x:fst c, snd c)
                            | otherwise = ("", xss)
                    where
                        c = manyHelper xs func

sT :: Char -> Bool
sT c = c `notElem` "<>"

sN :: Char -> Bool
sN c = isDigit c || isLetter c || c `elem` ".-"

cV :: Char -> Bool
cV c = c /= '\"'

manyT :: String -> (String,String)
manyT xss = manyHelper xss sT

-- Задача 2.b ----------------------------------------- 
value :: String -> (String,String)
value xss = manyHelper xss cV

-- Задача 2.c ----------------------------------------- 
manyN :: String -> (String,String)
manyN xss = manyHelper xss sN

-- Задача 3.a -----------------------------------------
nameHelper :: String -> (String -> (String,String)) -> (Char -> Bool) -> Maybe(String,String)
nameHelper "" _ _ = Nothing
nameHelper (x:xs) func bool = if bool x
                then Just (x:fst c, snd c)
                else Nothing
            where c = func xs


name :: String ->  Maybe(String,String)
name xs = nameHelper xs manyN isLetter

-- Задача 3.b -----------------------------------------
text :: String ->  Maybe(String,String)
text xs = nameHelper xs manyT sT

-- Задача 3.c -----------------------------------------
nameHelper2 :: String -> (String -> (String,String)) -> (Char -> Bool) -> Maybe(String,String)
nameHelper2 "" _ _ = Nothing
nameHelper2 (x:xs) func bool = if bool x
                then if null . snd $ c
                    then Nothing
                    else Just (fst c, tail . snd $ c)
                else Nothing
            where c = func xs

fullValue :: String ->  Maybe(String,String)
fullValue xs = nameHelper2 xs value (not . cV)

-- Задача 4.a -----------------------------------------
attrib :: String -> Maybe ((String,String),String)
attrib xs = case name xs of
                Nothing -> Nothing
                Just (a, b) ->
                    do -- a=name, b =spzes + '=' + spaces + fullVal + spaces
                        let c = spaces b -- c = '=' + spaces + fullVal + spaces
                        let e = fullValue (spaces (tail c)) -- e = fullVal + spaces
                        if head c == '='
                            then
                                case e of
                                    Nothing -> Nothing
                                    Just(f, g) -> Just ((a, f), spaces g) -- g == spaces + suffix
                            else Nothing

-- Задача 4.b -----------------------------------------
manyAtt :: String -> Maybe (Attributes,String)
manyAtt xs = case a of
                Nothing -> Nothing
                Just (b, c) -> do
                                let d = manyAtt (spaces c)
                                case d of
                                    Nothing -> Just ([b], c)
                                    Just (e, f) -> Just (b:e, f)
    where   a = attrib xs

-- Задача 5.a -----------------------------------------
begTag :: String -> Maybe ((String,Attributes),String)
begTag "" = Nothing
begTag (x:xs) = if x /= '<' then Nothing
                else case name xs of
                        Nothing -> Nothing
                        Just (a, b) -> case manyAtt (spaces b) of
                                        Nothing -> if head (spaces b) /= '>'
                                                        then Nothing
                                                        else Just ((a, []), tail b)
                                        Just (c, d) -> if head d /= '>'
                                                        then Nothing
                                                        else Just ((a, c), tail d)

-- Задача 5.b -----------------------------------------
startsWith :: String -> String -> Bool
startsWith _ "" = True
startsWith "" _ = False
startsWith (x:a) (y:b) = x == y &&  startsWith a b

endTag :: String -> Maybe (String,String)
endTag xs = if xs `startsWith` "</"
            then case name (tail (tail xs)) of
                Nothing -> Nothing
                Just (a, b) -> if head b == '>'
                                then Just (a, tail b)
                                else Nothing
            else Nothing

-- Задача 6.a -----------------------------------------
element :: String -> Maybe (XML,String)
element xs = case begTag xs of
                Nothing -> Nothing
                Just ((a, b), c) -> -- a = name, b = attr
                    case manyXML c of
                        Nothing -> Nothing
                        Just (d, e) -> -- d = [XML]
                            case endTag e of
                                Nothing -> Nothing
                                Just (f, g) ->
                                    if a == f then Just (Element a b d, g)
                                    else Nothing

-- Задача 6.b -----------------------------------------
xml :: String -> Maybe (XML,String)
xml xs = case element xs of
            Nothing -> case text xs of
                        Nothing -> Nothing
                        Just (a, b) -> Just (Text a, b)
            Just (c, d) -> Just (c, d)


-- Задача 6.c -----------------------------------------
manyXML :: String -> Maybe ([XML],String)
manyXML xs = case xml xs of
                Nothing -> Just ([], xs)
                Just (a, b) ->  case manyXML b of
                                Nothing ->      if b `startsWith` "</"
                                            then Just ([a], b)
                                            else Nothing
                                Just (c, d) ->  if d `startsWith` "</" 
                                            then Just (a:c, d)
                                            else Nothing

-- Задача 7 -----------------------------------------
fullXML :: String -> Maybe XML
fullXML xs = case element (spaces xs) of
                Nothing -> Nothing
                Just (a, b) -> if spaces b == "" then Just a
                                else Nothing

-- Тестові дані -------------------------------------------
-- Прості тести XML-об'єктів (без проміжків)
stst1, stst2, stst3 :: String
stst1 = "<a>A</a>"
stst2 = "<a x=\"1\"><b>A</b><b>B</b></a>"
stst3 = "<a>\
      \<b>\
        \<c att=\"att1\">text1</c>\
        \<c att=\"att2\">text2</c>\
      \</b>\
      \<b>\
        \<c att=\"att3\">text3</c>\
        \<d>text4</d>\
      \</b>\
    \</a>"

-- Результати аналізу попередніх XML-об'єктів
x1, x2, x3 :: XML
x1 = Element "a" [] [Text "A"]
x2 = Element "a"
            [("x","1")]
            [Element "b" [] [Text "A"],
             Element "b" [] [Text "B"]]
x3 = Element "a"
            []
            [Element "b"
                     []
                     [Element "c"
                              [("att","att1")]
                              [Text "text1"],
                      Element "c"
                              [("att","att2")]
                              [Text "text2"]],
             Element "b"
                     []
                     [Element "c"
                              [("att","att3")]
                              [Text "text3"],
                      Element "d"
                              []
                              [Text "text4"]]]

casablanca :: String
casablanca
  = "<film title=\"Casablanca\">\n  <director>Michael Curtiz</director>\n  <year>1942\
    \</year>\n</film>\n\n\n"

casablancaParsed :: XML
casablancaParsed
  = Element "film"
            [("title","Casablanca")]
            [Text "\n  ",
             Element "director" [] [Text "Michael Curtiz"],
             Text "\n  ",
             Element "year" [] [Text "1942"],
             Text "\n"]