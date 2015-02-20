module Combos where

import Cards

import Data.List

--Die 5 Karten, die fuer jeden Spieler gewertet werden
data Combo = Combo (Card,Card,Card,Card,Card)

--Die Verschiedenen Typen von Wertungen
data ScoreCombo = HighCard Card
                | Pair (Card,Card) 
                | Pair2 ((Card,Card),(Card,Card))
                | ThreeC (Card,Card,Card)
                | Straight (Card,Card,Card,Card,Card)
                | Flush (Card,Card,Card,Card,Card)
                | FullHouse (Card,Card,Card,Card,Card)
                | FourC (Card,Card,Card,Card)
                | StraightFlush (Card,Card,Card,Card,Card)
                | RoyalFlush (Card,Card,Card,Card,Card)
    deriving (Ord, Eq)

instance Show ScoreCombo where
    show (HighCard c) = "High Card " ++ show c
    show (Pair (c1,c2)) = "One Pair (" ++ show c1 ++ "," ++ show c2 ++")"
    show (Pair2 ((c1,c2),(c3,c4))) = "Two Pair (" ++ show c1 ++ "," ++ show c2 ++") and (" ++ show c3 ++ "," ++ show c4 ++")"
    show (ThreeC (c1,c2,c3)) = "Three of a Kind (" ++ show c1 ++ "," ++ show c2 ++ "," ++ show c3 ++")"
    show (Straight (c1,c2,c3,c4,c5)) = "Straight (" ++ show c1 ++ "," ++ show c2 ++ "," ++ show c3 ++ "," ++ show c4 ++ "," ++ show c5 ++")"
    show (Flush (c1,c2,c3,c4,c5)) = "Flush (" ++ show c1 ++ "," ++ show c2 ++ "," ++ show c3 ++ "," ++ show c4 ++ "," ++ show c5 ++")"
    show (FullHouse (c1,c2,c3,c4,c5)) = "Full House! Three (" ++ show c1 ++ "," ++ show c2 ++ "," ++ show c3 
                                        ++ ") and Two (" ++ show c4 ++ "," ++ show c5 ++")"
    show (FourC (c1,c2,c3,c4)) = "Four of a Kind ("++ show c1 ++ "," ++ show c2 ++ "," ++ show c3 ++ "," ++ show c4 ++ ")" 
    show (StraightFlush (c1,c2,c3,c4,c5)) = "Straight Flush (" ++ show c1 ++ "," ++ show c2 ++ "," ++ show c3 ++ "," ++ show c4 ++ "," ++ show c5 ++")"
    show (RoyalFlush (c1,c2,c3,c4,c5)) = "Royal Flush " ++ show c1 ++ "," ++ show c2 ++ "," ++ show c3 ++ "," ++ show c4 ++ "," ++ show c5 ++")"

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

        ind5 :: [Int] -> Int
        ind5 [] = 1
        ind5 (h:is) = if h>=5 then 0 else 1+ind5 is

        --getFlush erzeugt einen Flush aus einer Liste von Karten. Darf nur aufgerufen werden, wenn es auch einen Flush gibt
        --(sonst wird ein falscher CFlush erzeugt)
        getFlush :: [Card] -> Int -> ScoreCombo
        getFlush cs i
                    | i==0 = getDFlush cs
                    | i==1 = getHFlush cs
                    | i==2 = getSFlush cs
                    | otherwise = getCFlush cs
            where
                getDFlush = undefined
                getHFlush = undefined
                getSFlush = undefined
                getCFlush = undefined
                   