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
    | checkThree cs = undefined --Full House ueberpruefen
    | checkTwo cs = undefined --Zweites Pair ueberpruefen
    | otherwise = undefined --High Card erstellen
    where
        checkFlush :: [Card] -> (Bool)
        checkFlush cs = if (length cs <5) then False else any (>=5) (colorsIn cs [])

        checkStraight :: [Card] -> Bool
        checkStraight cs = if (length cs <5) then False else True --Brauche Strassen-Finde-Funktion

        checkFour :: [Card] -> Bool
        checkFour cs = if (length cs <4) then False else True --Brauche Vierling Finde Funktion

        checkThree :: [Card] -> Bool
        checkThree cs = if (length cs <3) then False else True --Brauche Drilling Finde Funktion

        checkTwo :: [Card] -> Bool
        checkTwo cs = if (length cs <2) then False else True --Brauche Paar Finde Funktion

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

        --zaehlt an welcher Stelle einer Int Liste ein Wert >= 5 ist und gibt diesen Index zurueck
        ind5 :: [Int] -> Int
        ind5 [] = 1
        ind5 (h:is) = if h>=5 then 0 else 1+ind5 is

        --getFlush erzeugt einen Flush aus einer Liste von Karten. Darf nur aufgerufen werden, wenn es auch einen Flush gibt
        --(sonst wird ein falscher CFlush erzeugt)
        getFlush :: [Card] -> Int -> ScoreCombo
        getFlush cs i
                    | i==0 = Flush $ getDFlush cs 0
                    | i==1 = Flush $ getHFlush cs 0
                    | i==2 = Flush $ getSFlush cs 0
                    | otherwise = Flush $ getCFlush cs 0
            where
                getDFlush [] _ = []
                getDFlush (ca:cs) x 
                    | x>=5 = []
                    | otherwise = if getColor ca == Diamonds then ca : getDFlush cs (x+1) else getDFlush cs x
                getHFlush [] _ = []
                getHFlush (ca:cs) x 
                    | x>=5 = []
                    | otherwise = if getColor ca == Hearts then ca : getHFlush cs (x+1) else getHFlush cs x
                getSFlush [] _ = []
                getSFlush (ca:cs) x 
                    | x>=5 = []
                    | otherwise = if getColor ca == Spades then ca : getSFlush cs (x+1) else getSFlush cs x
                getCFlush [] _ = []
                getCFlush (ca:cs) x 
                    | x>=5 = []
                    | otherwise = if getColor ca == Clubs then ca : getCFlush cs (x+1) else getCFlush cs x