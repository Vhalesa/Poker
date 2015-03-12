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
handValue [] = 0

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
tableValue cs = (bonusScoreChance cs) + comboValueScore kiCombo
    where
        kiCombo = checkCombo cs

cardListValue :: [Card] -> Int
cardListValue [] = 0
cardListValue (c:cs) = cardValueScore (getValue c) + cardListValue cs

-- Berechnet mit ein, dass evtl. noch die Chance auf einen Flush, eine Straigt oder ähnliches besteht
bonusScoreChance cs = undefined -- TODO alle Berechnungen hier nebenlaeufig verwalten und addieren

-- Berechnet abhaengig von der Moeglichkeit auf einen Flush einen Bonus Score
calculateFlushBonusScore :: [Card] -> Int
calculateFlushBonusScore cs
    | calculateFlushChance cs >= 0.19 && calculateFlushChance cs < 1.0 = 5000
    | calculateFlushChance cs >= 0.1 && calculateFlushChance cs < 0.19 = 2000
    | calculateFlushChance cs >= 0.05 && calculateFlushChance cs < 0.1 = 1000
    | otherwise = 0

--berechnet die Chance, dass noch ein Flush zusammen kommt
calculateFlushChance :: [Card] -> Double
calculateFlushChance cs
    | any (>=5) $ colorsIn cs [] = 1.0
    | any (==4) $ colorsIn cs [] = if length cs == 6 then 9/46 else if length cs == 5 then 9/47 + 9/46 else 0.0
    | any (==3) $ colorsIn cs [] = if length cs == 5 then 10/47 * 9/46 else 0.0
    | any (==2) $ colorsIn cs [] = if length cs == 2 then 3 * 11/50 * 10/49 * 9/48 else 0.0
    | otherwise = 0.0

-- Vermindert den Bonus Score abhaengig von der Moeglichkeit, dass noch hoehere Karten kommen
calculateOvercardBonusScore :: [Card] -> Int
calculateOvercardBonusScore cs = -20 * round (100 * calculateHigherCardChance cs)

-- Berechnet die Chance, dass noch eine hoehere Karte kommt, als die momentan hoechste
calculateHigherCardChance :: [Card] -> Double
calculateHigherCardChance cs
    | length cs >= 7 || highestV == Ace = 0.0
    | highestV == King = cardChance
    | highestV == Queen = 2 * cardChance
    | highestV == Jack = 3 * cardChance
    | highestV == Ten = 4 * cardChance
    | highestV == Nine = 5 * cardChance
    | highestV == Eight = 6 * cardChance
    | highestV == Seven = 7 * cardChance
    | highestV == Six = 8 * cardChance
    | highestV == Five = 9 * cardChance
    | highestV == Four = 10 * cardChance
    | highestV == Three = 11 * cardChance
    | highestV == Two = 12 * cardChance

    where
        highestV = getValue $ head cs
        remainingCards = 52 - (fromIntegral $ length cs)
        cardChance = 4/remainingCards

calculateStraightBonusScore :: [Card] -> Int
calculateStraightBonusScore cs
    | calculateStraightChance cs >= 0.15 && calculateStraightChance cs < 1.0 = 5000
    | calculateStraightChance cs >= 0.10 && calculateStraightChance cs < 0.15 = 3000
    | calculateStraightChance cs >= 0.05 && calculateStraightChance cs < 0.10 = 1000
    | otherwise = 0

-- Berechnet die Chance auf eine Strasse
calculateStraightChance :: [Card] -> Double
calculateStraightChance cs
    | length cs >= 7 = 0.0
    | maximumStraightComponents == 4 = 1.0
    | maximumStraightComponents == 3 = possible3 * cardChance
    | maximumStraightComponents == 2 = if length cs <= 5 then possible2 * cardChance * cardChance else 0.0
    | maximumStraightComponents == 1 = if length cs <= 4 then possible1 * cardChance * cardChance * cardChance else 0.0
    | otherwise = 0.0
    where
        existingStraightComponents = map (numberOfPreds cs) cs
        maximumStraightComponents = maximum existingStraightComponents
        remainingCards = 52 - (fromIntegral $ length cs)
        possible3 = fromIntegral $ length $ filter (==3) existingStraightComponents
        possible2 = fromIntegral $ length $ filter (==2) existingStraightComponents
        possible1 = fromIntegral $ length $ filter (==1) existingStraightComponents
        cardChance = 4/remainingCards