module Combos where

import Cards

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
    --ToDo Ord und Eq manuell implementieren

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