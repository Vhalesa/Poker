module Combos where

import Cards

import Data.List

--Die 5 Karten, die fuer jeden Spieler gewertet werden
data Combo = Combo (Card,Card,Card,Card,Card)

--Die Verschiedenen Typen von Wertungen
data ScoreCombo = HighCard [Card]
                | Pair [Card] 
                | Pair2 [Card]
                | ThreeC [Card]
                | Straight [Card]
                | Flush [Card]
                | FullHouse [Card]
                | FourC [Card]
                | StraightFlush [Card]
                | RoyalFlush [Card]
    deriving (Show, Ord, Eq)

--Ueberpruefe, welche Combo ein Spieler mit seiner Hand und den Tischkarten hat
checkCombo :: [Card] -> ScoreCombo
checkCombo cs
    | checkFlush cs = if (checkStraight $ cs) then undefined --noch ueberpruefen, ob royal

                      else getFlush cs $ ind5 $ colorsIn cs []
    | checkStraight cs = undefined --Strasse erstelle
    | checkFour cs = undefined --Vierling erstellen
    | fst $ checkThree cs = if (checkDifferentTwo cs (snd $ checkThree cs)) then getFullHouse cs --Full House erstellen
                            else undefined --Drilling erstellen
    | fst $ checkTwo cs = if (checkDifferentTwo cs (snd $ checkTwo cs)) then undefined else undefined--Zweites Pair ueberpruefen
    | otherwise = HighCard $ first5 cs --High Card erstellen
    where
        checkFlush :: [Card] -> (Bool)
        checkFlush cs = if (length cs <5) then False else any (>=5) (colorsIn cs [])

        checkStraight :: [Card] -> Bool
        checkStraight cs = if (length cs <5) then False else True --Brauche Strassen-Finde-Funktion

        checkFour :: [Card] -> Bool
        --Da Liste geordnet, muss nur der erste Wert mit den 3 Nachfolgern verglichen werden
        checkFour (c1:c2:c3:c4:cs) = if (c1==c2 && c1==c3 && c1==c4) then True else checkFour (c2:c3:c4:cs)
        checkFour _ = False

        checkThree :: [Card] -> (Bool,Card)
        --Da Liste geordnet, muss nur der erste Wert mit den 2 Nachfolgern verglichen werden
        checkThree (c1:c2:c3:cs) = if (c1==c2 && c1==c3) then (True,c1) else checkThree (c2:c3:cs)
        checkThree _ = (False,Card (Spades,Ace))

        checkTwo :: [Card] -> (Bool,Card)
        --Da Liste geordnet, muss nur der erste Wert mit dem Nachfolger verglichen werden
        checkTwo (c1:c2:cs) = if (c1==c2) then (True,c1) else checkTwo (c2:cs)
        checkTwo _ = (False,Card (Spades,Ace))

        --Ueberprueft, ob noch ein zweites Paar, das unterschiedlich zur uebergebenen Karte ist, existiert
        checkDifferentTwo :: [Card] -> Card -> Bool
        --Da Liste geordnet, muss nur der erste Wert mit dem Nachfolger verglichen werden
        checkDifferentTwo (c1:c2:cs) cx= if (c1==c2 && c1/=cx) then True else checkDifferentTwo (c2:cs) cx
        checkDifferentTwo _ _= False

        --zaehlt an welcher Stelle einer Int Liste ein Wert >= 5 ist und gibt diesen Index zurueck
        ind5 :: [Int] -> Int
        ind5 [] = 1
        ind5 (h:is) = if h>=5 then 0 else 1+ind5 is

        first5 :: [a] -> [a]
        first5 (a1:a2:a3:a4:a5:as) = (a1:a2:a3:a4:a5:[])
        first5 _ = []

        --getFlush erzeugt einen Flush aus einer Liste von Karten. Darf nur aufgerufen werden, wenn es auch einen Flush gibt
        --(sonst wird ein leerer CFlush erzeugt)
        getFlush :: [Card] -> Int -> ScoreCombo
        getFlush cs i
                    | i==0 = Flush $ first5 $ getDFlush cs
                    | i==1 = Flush $ first5 $ getHFlush cs
                    | i==2 = Flush $ first5 $ getSFlush cs
                    | otherwise = Flush $ first5 $ getCFlush cs
            where
                getDFlush [] = []
                getDFlush (ca:cs) = if getColor ca == Diamonds then ca : getDFlush cs else getDFlush cs
                getHFlush [] = []
                getHFlush (ca:cs) = if getColor ca == Hearts then ca : getHFlush cs else getHFlush cs
                getSFlush [] = []
                getSFlush (ca:cs) = if getColor ca == Spades then ca : getSFlush cs else getSFlush cs
                getCFlush [] = []
                getCFlush (ca:cs) = if getColor ca == Clubs then ca : getCFlush cs else getCFlush cs

        --getFullHouse erzeugt ein Full House aus einer Liste von Karten
        getFullHouse :: [Card] -> ScoreCombo
        getFullHouse cs = FullHouse $ threes ++ twoes
            where
                threes = get3 cs
                twoes = get2Different cs $ head threes

        --Gibt einen Drilling als Liste von Karten
        get3 :: [Card] -> [Card]
        get3 (c1:c2:c3:cs) = if c1==c2 && c1==c3 then [c1,c2,c3] else get3 (c2:c3:cs)
        get3 _ = []

        --Gibt ein Kartenpaar als Liste von Karten
        get2 :: [Card] -> [Card]
        get2 (c1:c2:cs) = if c1==c2 then [c1,c2] else get3 (c2:cs)
        get2 _ = []

        --Gibt ein Kartenpaar als Liste, das nicht gleich ist wie die uebergebene Karte cx
        get2Different :: [Card] -> Card -> [Card]
        get2Different (c1:c2:cs) cx = if (c1==c2 && c1/=cx) then [c1,c2] else get2Different (c2:cs) cx
        get2Different _ _= []

-- Diese Funktion zaehlt wie oft welche Farbe in einer Liste an Karten vorkommt.
-- Format: [Diamond , Hearts , Spades , Clubs]
colorsIn :: [Card] -> [Int] -> [Int]
colorsIn [] is = is
colorsIn (ca : cs) (d:h:s:c:is) 
            | getColor ca == Diamonds = colorsIn cs (d+1:h:s:c:[])
            | getColor ca == Hearts = colorsIn cs (d:h+1:s:c:[])
            | getColor ca == Spades = colorsIn cs (d:h:s+1:c:[])
            | otherwise = colorsIn cs (d:h:s:c+1:[])
colorsIn cs _ = colorsIn cs [0,0,0,0]