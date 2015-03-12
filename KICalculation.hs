module KICalculation where

import Data.List
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad
import System.Random
import System.IO

import Combos
import Cards

checkKICardValue :: [Card] -> IO Int
checkKICardValue cs = if length cs < 5 then return (handValue cs) else tableValue cs

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
tableValue :: [Card] -> IO Int
tableValue cs = do
    bonus <-(bonusScoreChance cs) 
    return $ bonus + comboValueScore kiCombo
    where
        kiCombo = checkCombo cs

cardListValue :: [Card] -> Int
cardListValue [] = 0
cardListValue (c:cs) = cardValueScore (getValue c) + cardListValue cs

-- Berechnet mit ein, dass evtl. noch die Chance auf einen Flush, eine Straigt oder ähnliches besteht
-- bekommt die Hand+Tischkarten uebergeben
-- TODO alle Berechnungen hier nebenlaeufig verwalten und addieren
bonusScoreChance :: [Card] -> IO Int
bonusScoreChance cs = do
  doneCalc <- newTVarIO []
  sequence_ [ forkIO $ chanceBerechnung n cs doneCalc | n <- [1..2]] -- hier muss bei n = Anzahl aller Berechnungen
  hSetBuffering stdin NoBuffering 
  erg <- warten doneCalc
  putStr "KI hat nebenläufig berechnet: "
  print erg
  return erg

--wartet bis alle Berechnungen fertig sind, addiert diese und gibt den errechnet Wert zurueck
warten :: TVar [Int] -> IO Int
warten doneCalc = do
  c <- atomically getCalc
  let chance = foldl (+) 0 c
  return chance
  where getCalc = do
          d <- readTVar doneCalc
          if (length d >= 2) --hier muss mit Anzahl aller Berechnungen verglichen werden
            then return ()
            else retry
          writeTVar doneCalc [] --Liste der fertigen Berechnungen leeren
          return d 
  
-- berechnet nebenlaeufig, was fuer eine Wahrscheinlichkeit ihr n hat und schreibt es in die TVar
chanceBerechnung :: Int -> [Card] -> TVar [Int] -> IO ()
chanceBerechnung n cs doneCalc = do
  let c = calculateChance n cs
  atomically $ do
    d <- readTVar doneCalc
    writeTVar doneCalc $ c : d 

-- Berechnet je nach n, die Wahrschneinlichkeit fuer Flush, Straight ...
calculateChance :: Int -> [Card] -> Int
calculateChance n cs
  | n == 1 = calculateFlushBonusScore cs
  | otherwise = calculateOvercardBonusScore cs

-- Berechnet abhaengig von der Moeglichkeit auf einen Flush einen Bonus Score
calculateFlushBonusScore :: [Card] -> Int
calculateFlushBonusScore cs
    | calculateFlushChance cs >= 0.19 && calculateFlushChance cs < 1.0 = 5000
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
    | highestV == King = 4/remainingCards
    | highestV == Queen = 8/remainingCards
    | highestV == Jack = 12/remainingCards
    | highestV == Ten = 16/remainingCards
    | highestV == Nine = 20/remainingCards
    | highestV == Eight = 24/remainingCards
    | highestV == Seven = 28/remainingCards
    | highestV == Six = 32/remainingCards
    | highestV == Five = 36/remainingCards
    | highestV == Four = 40/remainingCards
    | highestV == Three = 44/remainingCards
    | highestV == Two = 48/remainingCards

    where
        highestV = getValue $ head cs
        remainingCards = 52 - (fromIntegral $ length cs)
