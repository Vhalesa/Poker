module Aktionen where

import Player
import Cards
import Combos

import Data.List

--p1 folded
fold :: ([Player],Int) -> ([Player],Int)
fold ((p1:ps),pot) = (ps ++ [setPlayerIngame False p1], pot)

-- p1 erhoeht 
-- bekommt die Liste der Player und den Pot (und gibt diese mit Veraenderung wieder zurueck)
call :: ([Player],Int) -> ([Player],Int)
call p = raise p 0 

-- p1 erhoeht um einen Betrag 
-- raise bekommt die Liste der Player, den Pot und den erhoehten Betrag 
-- Reihenfolge der Spieler wird um 1 verschoben -> naechster Spieler ist dran
raise :: ([Player],Int) -> Int -> ([Player],Int)
raise ((p1:p2:ps),pot) betrag 
  -- genung Geld fuer die Raise (bzw. fuer Call)
  | eigCash >= diff                        = (p2:ps ++ [pay diff p1], pot + diff) 
  -- nicht genug Geld fuer den kompletten Raise, aber fuer Call und einen Teil des Raise (es wird AllIn gesetzt) 
  | eigCash >= diffBets && diff >= eigCash = (p2:ps ++ [pay eigCash p1], pot + eigCash) 
  -- wenn nicht genug Geld fuer Call -> anderer Spieler bekommt sein zuviel gezahltes Geld zurueck, auch vom Pot wird es abgezogen
  | otherwise                              = moneyBack p1 (p2:ps) [] pot 
    where diff       = diffBets + betrag
          diffBets   = (maximum (map getCurrentBet (p1:p2:ps))) - getCurrentBet p1 
          eigCash    = getPlayerCash p1
          moneyBack :: Player -> [Player] -> [Player] -> Int -> ([Player],Int)
          moneyBack p1 [] erg pot = (erg ++ [pay eigCash p1], pot)
          moneyBack p1 (p2:ps) erg pot = if ((getCurrentBet p2 - (eigCash + getCurrentBet p1)) < 0)
                                            then moneyBack p1 ps (erg ++ [p2]) pot
                                            else moneyBack p1 ps 
                                                 (erg ++ [pay (eigCash - (getCurrentBet p2 - getCurrentBet p1)) p2])
                                                 (pot - eigCash - (getCurrentBet p2 - getCurrentBet p1)) 

-- Spieler bezahlt aus seinem Geld einen bestimmten Betrag
pay :: Int -> Player -> Player
pay 0 p = p 
pay x p = p {cash = getPlayerCash p - x, currentBet = getCurrentBet p + x}

--Kuemmert sich um alles, was die Blinds angeht, Blinds zuweisen, bezahlen und in den Pot packen.
doBlinds :: [Player] -> Int -> IO([Player],Int)
doBlinds ps x = do
  let ps1 = delegateBlind ps
      ps2 = map (payBlind x) ps1 
      pot = blinds x
  putStrLn ("Der Pot betraegt nach den Blinds " ++ show pot)
  putStrLn ("Verbleibende Chips: " ++ show ps2)
  return (ps2,pot)
                                    
-- Small und Big Blind zuweisen
delegateBlind :: [Player] -> [Player]
delegateBlind [] = []
delegateBlind (p:ps)
    | getPlayerRole p == BigBlind   = p {role = SmallBlind} : delegateBlind ps
    | getPlayerRole p == SmallBlind = delegateBlind ps ++ [p {role = BigBlind}]
    | otherwise                     = p : delegateBlind ps

-- Blinds kommen in den Pot; Uebergabeparameter = Small Blind
blinds :: Int -> Int 
blinds v = 3*v 

-- Spieler muss Blind bezahlen Uebergabeparameter = Small Blind
payBlind :: Int -> Player -> Player
payBlind v p 
    | getPlayerRole p == BigBlind   = pay (2*v) p
    | getPlayerRole p == SmallBlind = pay v p
    | otherwise                     = p

-- Der CurrentBet aller Spieler wird wieder auf 0 gesetzt (vor jeder Setzrunde erforderlich)
resetBets :: [Player] -> [Player]
resetBets ps = map removeCurrentBet ps

-- Handkarten der Spieler werden zurueckgesetzt (wieder leere Hand)
resetHands :: [Player] -> [Player]
resetHands ps = map (setPlayerHand []) ps

-- Combos werden zurueckgesetzt (wieder keine)
resetCombos :: [Player] -> [Player]
resetCombos ps = map (setPlayerCombo $ HighCard []) ps

-- alle Spieler spielen wieder mit (falls jemand fold eingesetzt hatte)
resetIngame :: [Player] -> [Player]
resetIngame ps = map (setPlayerIngame True) ps

-- Zieht n mal jeweils x Karten, und gibt auch den Rest des Decks zurueck [[c1][c2]...[rest]]
-- Kann mit n = 1 genutzt werden, um Karten zum aufdecken zu ziehen
-- kann mit n >= 1 und x = 2 genutzt werden, um Spielern die Startkarten zu ziehen
-- Sollte aufgerufen werden: austeilen deck n [] x;
--  wenn erg /= [] werden die Karten in erg am Ende wieder mit ausgegeben
austeilen :: [Card] -> Int -> [[Card]] -> Int -> [[Card]]
austeilen deck 0 erg x = erg ++ [deck]
austeilen deck n erg x = austeilen (snd $ splitAt x deck) (n-1) (erg ++ [(fst $ splitAt x deck)]) x

-- Vervollstaendigt die Tischkarten auf 5. 
completeTableCards :: [Card] -> [Card] -> [Card]
completeTableCards deck table = head $ austeilen deck 1 [] (5 - length table)

-- Rund1,2,3,4 (jeweils das Karteaufdecken + Aufruf von runde)
-- mit Ausgabe, welche Karte gezogen wurde
--
-- 2 Karten werden fuer jeden Spieler gezogen
--runde1 :: [Card] -> [Player] -> IO
runde1 stapel p = do
  let cards1 = austeilen stapel 2 [] 2
      p1 = setPlayerHand (head cards1) (head p)
      p2 = setPlayerHand (cards1 !! 1) (p !! 1)
  putStrLn ("Jeder Spieler hat seine Karten auf die Hand bekommen")
  return ([p1,p2], last cards1)

--runde1 nicht in IO. 
runde1b :: [Card] -> [Player] -> ([Player],[Card])
runde1b cs ps = ([p1,p2],last cards1)
  where cards1 = austeilen cs 2 [] 2
        p1 = setPlayerHand (head cards1) (head ps)
        p2 = setPlayerHand (cards1 !! 1) (ps !! 1)
                                                            
-- 3 Karten werden vom Stapel genommen
runde2b :: [Card] -> [[Card]]
runde2b cs = austeilen cs 1 [] 3

-- 1 Karte wird vom Stapel genommen (Funktioniert fuer Runde 3 und 4)
runde34 :: [Card] -> [[Card]]
runde34 cs = austeilen (last trashCard) 1 [] 1
  where
  --TrashCard ist die Oberste Karte, die laut Regeln vor jedem geben zur Seite gelegt wird
  trashCard = austeilen cs 1 [] 1

--Gibt die ersten 3 Karten, den sog. Flop zurueck, sowie das restliche Deck
runde2 cs = do
  let
    --TrashCard ist die Oberste Karte, die laut Regeln vor jedem geben zur Seite gelegt wird
    trashCard = austeilen cs 1 [] 1
    cardsAndDeck = runde2b (last trashCard)
  putStrLn ""
  putStrLn("Der Flop ist " ++ show (head cardsAndDeck))
  return cardsAndDeck

--Gibt die vierte Karte, den sog. Turn zurueck, sowie das restliche Deck
runde3 cs = do
  let cardsAndDeck = runde34 cs
  putStrLn ""
  putStrLn("Die Turn Karte ist " ++ show (head cardsAndDeck))
  return cardsAndDeck

--Gibt die fuenfte und letzte Karte, den sog. River, zurueck, OHNE das restliche Deck (Das brauchen wir nicht mehr)
runde4 cs = do
  let cardsAndDeck = runde34 cs
  putStrLn ""
  putStrLn("Die River Karte ist " ++ show (head cardsAndDeck))
  return cardsAndDeck

-- Showdown
showdown :: ([Player],Int) -> [Card] -> IO [Player]
showdown (ps,pot) cs = do
  let playersWithCombo = map (getComboForPlayer cs) ps
      winner = playerWithHighestCombo playersWithCombo
      updatedPlayers = payWinner playersWithCombo (winner,pot)
  putStrLn("Gewonnen hat " ++ (show (map getPlayerName winner)) ++ " mit " ++ show (map getPlayerCombo winner) ++ "")
  putStrLn( show updatedPlayers)
  return updatedPlayers

-- Gewinner bezahlt negativen Betrag = Gewinner bekommt Betrag  
payWinner :: [Player] -> ([Player],Int) -> [Player]
payWinner all (winners,pot) = replace newWinners all
  where newWinners = map (pay (negate $ quot pot (length winners))) winners

-- Gibt den Spieler (bzw. die Spieler) mit der hoechsten Combo aus
-- Funktioniert fuer beliebig viele Spieler, bis auf die letzte Zeile
playerWithHighestCombo :: [Player] -> [Player]
playerWithHighestCombo [p1] = [p1]
playerWithHighestCombo (p1:p2:ps)
  | getPlayerCombo p1 > getPlayerCombo p2 = playerWithHighestCombo (p1:ps)
  | getPlayerCombo p1 < getPlayerCombo p2 = playerWithHighestCombo (p2:ps)
  | otherwise = if all ( == getPlayerCombo p1 ) (map getPlayerCombo (p2:ps)) then (p1:p2:ps) else playerWithHighestCombo $ (p1:ps) ++ [p2]

-- Ersetzt in der zweiten Liste die Spieler mit gleichem Namen wie in der ersten Liste.
replace :: [Player] -> [Player] -> [Player]
replace ns as = helpReplace ns as []
    where
        helpReplace ns [] vs = helpReplace ns vs []
        helpReplace [] as vs = as ++ vs
        helpReplace (n:ns) (a:as) vs = if (n==a) then n : helpReplace ns as vs else helpReplace (n:ns) as $ vs ++ [a] 
