module KICalculation where

import Combos
import Cards

checkKICardValue :: [Card] -> Int
checkKICardValue cs = if length cs < 5 then handValue cs else tableValue cs

-- fiktiver Int Wert der die Staerke der Hand der KI angibt
handValue :: [Card] -> Int
handValue (c1:c2:cs)
    -- Zwei Karten mit gleichem Wert
    | c1 == c2 = 1000 + cardValueScore (getValue c1) + cardValueScore (getValue c2)
    -- Zwei Karten gleicher Farbe
    | getColor c1 == getColor c2 = 200 + cardValueScore (getValue c1) + cardValueScore (getValue c2)
    -- Zwei aufeinander folgende Karten
    | predInList c1 (c2:cs) || predInList c2 (c1:cs) = 100 + cardValueScore (getValue c1) + cardValueScore (getValue c2)
    -- Sonstiges
    | otherwise = cardValueScore (getValue c1) + cardValueScore (getValue c2)
handValue (c1:cs) = cardValueScore (getValue c1)

-- Ein fiktiver Wert, mit dem die KI berechnet, wie gut ihre Hand ist
cardValueScore :: Value -> Int
cardValueScore Ace = 800
cardValueScore King = 700
cardValueScore Queen = 650
cardValueScore Jack = 600
cardValueScore Ten = 550
cardValueScore Nine = 500
cardValueScore Eight = 400
cardValueScore Seven = 300
cardValueScore Six = 200
cardValueScore Five = 150
cardValueScore Four = 100
cardValueScore Three = 50
cardValueScore Two = 0

comboValueScore :: ScoreCombo -> Int
comboValueScore (RoyalFlush cs)= 45000
comboValueScore (StraightFlush cs)= 40000 + cardListValue cs
comboValueScore (FourC cs)= 35000 + cardListValue cs
comboValueScore (FullHouse cs)= 30000 + cardListValue cs
comboValueScore (Flush cs)= 25000 + cardListValue cs
comboValueScore (Straight cs)= 20000 + cardListValue cs
comboValueScore (ThreeC cs)= 15000 + cardListValue cs
comboValueScore (Pair2 cs)= 10000 + cardListValue cs
comboValueScore (Pair cs)= 5000 + cardListValue cs
comboValueScore (HighCard cs)= cardListValue cs

-- Berechnung der KI um ihre Hand zu analysieren
tableValue :: [Card] -> Int
tableValue cs = comboValueScore kiCombo
    where
        kiCombo = checkCombo cs

cardListValue :: [Card] -> Int
cardListValue [] = 0
cardListValue (c:cs) = cardValueScore (getValue c) + cardListValue cs

