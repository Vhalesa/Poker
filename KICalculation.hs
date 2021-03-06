module KICalculation where

import Control.Concurrent.STM
import Control.Concurrent
import System.Random

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
-- und gibt diesen BonusScore zurueck
-- bekommt die Hand+Tischkarten uebergeben
bonusScoreChance :: [Card] -> IO Int
bonusScoreChance cs = do
  -- hier kommen alle BonusScores der fertigen Berechnungen rein
  doneCalc <- newTVarIO []
  sequence_ [ forkIO $ chanceBerechnung n cs doneCalc | n <- [1..8]] -- hier muss bei n = Anzahl aller Berechnungen
  erg <- warten doneCalc
  return erg

--wartet bis alle Berechnungen fertig sind, addiert diese und gibt den errechnet Wert zurueck
warten :: TVar [Int] -> IO Int
warten doneCalc = do
  listChance <- atomically getCalc
  let addChance = foldl (+) 0 listChance -- addiert alle einzelnen BonusScores
  return addChance
  -- ueberprueft, ob alle Berechnungen fertig sind, wenn ja gibt sie sie zurueck
  where getCalc = do
          dCalc <- readTVar doneCalc
          if (length dCalc >= 8) --hier muss mit Anzahl aller Berechnungen verglichen werden
            then return ()
            else retry --solange versuchen, bis alle fertig sind
          writeTVar doneCalc [] --Liste der fertigen Berechnungen leeren (fuers naechste Mal)
          return dCalc 
  
-- berechnet nebenlaeufig, was fuer eine Wahrscheinlichkeit/BonusScore ihr n (Flush, Straight...) hat und schreibt es in die TVar
chanceBerechnung :: Int -> [Card] -> TVar [Int] -> IO ()
chanceBerechnung n cs doneCalc = do
  let singleChance = calculateChance n cs
  atomically $ do
    d <- readTVar doneCalc
    writeTVar doneCalc $ singleChance : d 

-- Berechnet je nach n, die Wahrschneinlichkeit/BonusScore fuer Flush, Straight ...
calculateChance :: Int -> [Card] -> Int
calculateChance n cs
  | n == 1    = calculateFlushBonusScore cs
  | n == 2    = calculateStraightBonusScore cs
  | n == 3    = calculatePairBonusScore cs
  | n == 4    = calculateTwoPairsBonusScore cs
  | n == 5    = calculateDrillingBonusScore cs
  | n == 6    = calculateVierlingBonusScore cs
  | n == 7    = calculateFullHouseBonusScore cs
  | otherwise = calculateOvercardBonusScore cs

-- Berechnet abhaengig von der Moeglichkeit auf einen Flush einen Bonus Score
calculateFlushBonusScore :: [Card] -> Int
calculateFlushBonusScore cs
    | calculateFlushChance cs >= 0.19 && calculateFlushChance cs < 1.0 = 5000
    | calculateFlushChance cs >= 0.1 = 2000
    | calculateFlushChance cs >= 0.05 = 1000
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

--Bonus Score fuer ein Paar 
calculatePairBonusScore :: [Card] -> Int
calculatePairBonusScore cs
    | calculatePairChance cs > 0.25 && calculatePairChance cs < 1.0 = 1000
    | calculatePairChance cs > 0.12 = 300
    | calculatePairChance cs > 0.0  = 100
    | otherwise = 0

--Berechnet die Chance, dass ein Paar zustande kommen kann
calculatePairChance :: [Card] -> Double
calculatePairChance cs
  | length cs >= 7 = 0.0 --es kommt keine weitere Karte mehr
  | any (>=2) $ map snd (valuesIn cs) = 1.0 --es gibt bereits ein Paar
  | otherwise = 1.0 - ((1 - cardChance2) ^ (7 - length cs)) 
  where cardChance2 = 3 / remainingCards
        remainingCards = 52 - (fromIntegral $ length cs)

--Bonus Score fuer ein 2 Paare
calculateTwoPairsBonusScore :: [Card] -> Int
calculateTwoPairsBonusScore cs
    | calculateTwoPairsChance cs > 0.25 && calculateTwoPairsChance cs < 1.0 = 2000
    | calculateTwoPairsChance cs > 0.12 = 600
    | calculateTwoPairsChance cs > 0.0  = 200
    | otherwise = 0

--Chance, dass noch 2 Paare zustande kommen
calculateTwoPairsChance :: [Card] -> Double
calculateTwoPairsChance cs
  | length cs >= 7 = 0.0
  | length (filter (>=2) $ map snd (valuesIn cs)) >= 2 = 1.0 --es gibt bereits 2 Paare
  | length (filter (>=2) $ map snd (valuesIn cs)) >= 1 = wahrsch $ (length cs) - 2 -- bereits ein Paar
  | length cs <= 5 = (wahrsch (length cs)) * (wahrsch $ (length cs)) --noch kein Paar
  | otherwise = 0.0 -- es werden keine 2 Karten mehr gezogen, und es gibt keine gleichwertigen Karten
  where cardChance2 = 3 / remainingCards 
        remainingCards = 52 - (fromIntegral $ length cs)
        wahrsch cardsDrawn = 1.0 - ((1 - cardChance2) ^ (7 - length cs))

--Bonus Score fuer ein Drilling
calculateDrillingBonusScore :: [Card] -> Int
calculateDrillingBonusScore cs
    | calculateDrillingChance cs > 0.20 && calculateDrillingChance cs < 1.0 = 3000
    | calculateDrillingChance cs > 0.10 = 1000 
    | calculateDrillingChance cs > 0.0  = 300
    | otherwise = 0

--Chance, dass noch ein Drilling zustande kommt
calculateDrillingChance :: [Card] -> Double
calculateDrillingChance cs
  | length cs >= 7 = 0.0 --es kommt keine weitere Karte mehr
  | any (>=3) $ map snd (valuesIn cs) = 1.0 --es gibt bereits ein Drilling 
  | any (>=2) $ map snd (valuesIn cs) = (wahrsch (length cs) cardChance3) * anzahlPaare --es gibt bereits mind. ein Paar
  | length cs <= 5 = (wahrsch (length cs) cardChance3) * (wahrsch (length cs) cardChance2) --noch gar nichts
  | otherwise = 0.0 
  where cardChance2 = 3 / remainingCards 
        cardChance3 = 2 / remainingCards 
        remainingCards = 52 - (fromIntegral $ length cs)
        wahrsch cardsDrawn cardChance = 1.0 - ((1 - cardChance) ^ (7 - length cs))
        anzahlPaare = (fromIntegral $ length (filter (>=2) (map snd (valuesIn cs)))) --Anzahl der Paare


--Bonus Score fuer ein FullHouse 
calculateFullHouseBonusScore :: [Card] -> Int
calculateFullHouseBonusScore cs
    | calculateFullHouseChance cs >= 0.20 && calculateFullHouseChance cs < 1.0 = 5000
    | calculateFullHouseChance cs >= 0.10 = 3000
    | calculateFullHouseChance cs >= 0.05 = 1000
    | otherwise = 0

--Chance, dass noch ein FullHouse zustande kommt
calculateFullHouseChance :: [Card] -> Double
calculateFullHouseChance cs
  | length cs >= 7 = 0.0 -- es kommt keine weitere Karte mehr
  | length (filter (>=3) (map snd (valuesIn cs))) >= 1 && length (filter (>=2) (map snd (valuesIn cs))) >= 2 =
    1.0 -- es gibt bereits ein FullHouse
  | any (>=3) $ map snd (valuesIn cs) = wahrsch ((length cs) - 3) cardChance2 --es gibt bereits ein Drilling
  | length (filter (>=2) (map snd (valuesIn cs))) >= 2 = (wahrsch (length cs) cardChance3) * anzahlPaare
     --es gibt mind. 2 Paare
  | (length cs <= 5) && (any (>=2) $ map snd (valuesIn cs)) = 
            (wahrsch (length cs) cardChance3) * (wahrsch (length cs) cardChance2) --es gibt bereits ein Paar
  | length cs <= 4 = calculateDrillingChance cs * calculatePairChance cs --kein Paar, es werden noch mind. 3 Karten gezogen 
  | otherwise = 0.0 
  where cardChance2 = 3 / remainingCards 
        cardChance3 = 2 / remainingCards 
        remainingCards = 52 - (fromIntegral $ length cs)
        wahrsch cardsDrawn cardChance = 1.0 - ((1 - cardChance) ^ (7 - length cs))
        anzahlPaare = (fromIntegral $ length (filter (>=2) (map snd (valuesIn cs)))) --Anzahl der Paare

--Bonus Score fuer ein Vierling 
calculateVierlingBonusScore :: [Card] -> Int
calculateVierlingBonusScore cs
    | calculateVierlingChance cs >= 0.03 && calculateVierlingChance cs < 1.0 = 2000
    | otherwise = 0

--Chance, dass noch ein Vierling zustande kommt
calculateVierlingChance :: [Card] -> Double
calculateVierlingChance cs
  | length cs >= 7 = 0.0 --es kommt keine weitere Karte mehr
  | any (>=4) $ map snd (valuesIn cs) = 1.0 --es gibt bereits ein Vierling 
  | any (>=3) $ map snd (valuesIn cs) = (wahrsch (length cs) cardChance4) * anzahlDrillinge--es gibt bereits mind. ein Drilling 
  | any (>=2) (map snd (valuesIn cs)) && length cs <= 5 = 
      (wahrsch (length cs) cardChance3) * (wahrsch (length cs) cardChance4) * anzahlPaare --es gibt bereits mind. ein Paar
  | length cs <= 4 = --kein Paar, 3(+) Karten werden gezogen 
      (wahrsch (length cs) cardChance2) * (wahrsch (length cs) cardChance3) * (wahrsch (length cs) cardChance4) 
  | otherwise = 0.0 
  where cardChance2 = 3 / remainingCards 
        cardChance3 = 2 / remainingCards 
        cardChance4 = 1 / remainingCards 
        remainingCards = 52 - (fromIntegral $ length cs)
        wahrsch cardsDrawn cardChance = 1.0 - ((1 - cardChance) ^ (7 - length cs))
        anzahlPaare = (fromIntegral $ length (filter (>=2) (map snd (valuesIn cs)))) --Anzahl der Paare
        anzahlDrillinge = (fromIntegral $ length (filter (>=3) (map snd (valuesIn cs)))) --Anzahl der Paare

--Bonus Score fuer die Strasse
calculateStraightBonusScore :: [Card] -> Int
calculateStraightBonusScore cs
    | calculateStraightChance cs >= 0.15 && calculateStraightChance cs < 1.0 = 5000
    | calculateStraightChance cs >= 0.10 = 3000
    | calculateStraightChance cs >= 0.05 = 1000
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
