module Aktionen where

import Player
import Cards
import Combos

import Data.List

-- Spieler p1 setzt fold ein und ist fuer diese Runde nicht mehr im Spiel
-- Naechster kommt an die Reihe 
fold :: ([Player],Int) -> ([Player],Int)
fold ((p1:ps),pot) = (ps ++ [setPlayerIngame False p1], pot)

-- Spieler p1 called. Naechster kommt an die Reihe.
-- bekommt die Liste der Player und den Pot (und gibt diese mit Veraenderung wieder zurueck)
call :: ([Player],Int) -> ([Player],Int)
call p = raise p 0 

-- p1 erhoeht um einen Betrag 
-- raise bekommt die Liste der Player, den Pot und den erhoehten Betrag 
-- Reihenfolge der Spieler wird um 1 verschoben -> naechster Spieler ist dran
raise :: ([Player],Int) -> Int -> ([Player],Int)
raise ((p1:p2:ps),pot) betrag 
  -- genug Geld fuer die Raise (bzw. fuer Call)
  | cashP1 >= diff                       = (p2:ps ++ [pay diff p1], pot + diff) 
  -- nicht genug Geld fuer den kompletten Raise, aber fuer Call und einen Teil des Raise (p1 zahlt sein gesamtes Geld ein)
  | cashP1 >= diffBets && diff >= cashP1 = (p2:ps ++ [pay cashP1 p1], pot + cashP1) 
  -- wenn nicht genug Geld fuer Call -> andere Spieler bekommen ihr zuviel gezahltes Geld zurueck, auch vom Pot wird es abgezogen
  | otherwise                            = moneyBack p1 (p2:ps) [] pot 
    where diff     = diffBets + betrag -- (Differenz zwischen eigener und hoechster Wette) + erhoeter Betrag
          diffBets = (maximum (map getCurrentBet (p1:p2:ps))) - getCurrentBet p1 --Differenz zwischen hoechster und eigener Wette
          cashP1   = getPlayerCash p1
          -- andere Spieler bekommen ihr zu viel gezahltes Geld zurueck, wenn ein Spieler AllIn gegangen ist
          -- auch vom Pot wird es wieder abgezogen
          moneyBack :: Player -> [Player] -> [Player] -> Int -> ([Player],Int)
          --am Schluss zahlt p1 sein gesamtes Geld und es kommt in den Pot
          moneyBack p1 [] erg pot = (erg ++ [pay cashP1 p1], pot + cashP1)
          --prueft fuer alle Mitspieler von p1, ob sie eine hoehere Wette haben als p1 (mit seinem gesamten Geld)
          moneyBack p1 (p2:ps) erg pot = if ((getCurrentBet p2 - (cashP1 + getCurrentBet p1)) <= 0)
                                            --p2 kriegt kein Geld zurueck
                                            then moneyBack p1 ps (erg ++ [p2]) pot
                                            --p2 bekommt Geld zurueck und der Pot auch
                                            else moneyBack p1 ps 
                                                 (erg ++ [pay (cashP1 - (getCurrentBet p2 - getCurrentBet p1)) p2])
                                                 (pot + cashP1 - (getCurrentBet p2 - getCurrentBet p1)) 

-- Spieler bezahlt aus seinem Geld einen bestimmten Betrag
pay :: Int -> Player -> Player
pay 0 p = p 
pay x p = p {cash = getPlayerCash p - x, currentBet = getCurrentBet p + x}

--Kuemmert sich um alles, was die Blinds angeht, Blinds zuweisen, bezahlen und in den Pot packen.
doBlinds :: [Player] -> Int -> IO([Player],Int)
doBlinds ps x = do
  let ps1 = delegateBlind ps
      ps2 = map (payBlind blind) ps1 
      pot = blinds blind
      blind = if any (<2*x) (map getPlayerCash ps) then quot (minimum $ map getPlayerCash ps) 2 else x
  putStrLn ("Der Pot betraegt nach den Blinds " ++ show pot)
  putStrLn ("Verbleibende Chips: " ++ show ps2)
  return (ps2,pot)
                                    
-- Small und Big Blind zuweisen
delegateBlind :: [Player] -> [Player]
delegateBlind [] = []
delegateBlind p
  | length p == 2 = reverse $ sort $ del2 p 
  | length p == 3 = reverse $ sort $ del3 p
  | length p > 3 = reverse $ sort $ reverse $ del $ sort p
  | otherwise     = p
    where del2 [] = [] -- fuer nur 2 Spieler (Big Blind und Small Blind)
          del2 (p:ps)
            | getPlayerRole p == BigBlind   = p {role = SmallBlind} : del2 ps
            | getPlayerRole p == SmallBlind = del2 ps ++ [p {role = BigBlind}]
            | otherwise = p : del2 ps

          del3 [] = [] -- fuer 3 Spieler (Big Blind, Small Blind und Dealer)
          del3 (p:ps)
            | getPlayerRole p == BigBlind   = p {role = SmallBlind} : del3 ps
            | getPlayerRole p == Dealer     = p {role = BigBlind} : del3 ps
            | getPlayerRole p == SmallBlind = del3 ps ++ [p {role = Dealer}]
            | otherwise = p : del3 ps
          
          del (p1:ps) -- fuer 4 oder mehr Spieler
            | getPlayerRole p1 == BigBlind   = p1 {role = SmallBlind} : del ps
            | getPlayerRole p1 == SmallBlind = p1 {role = Dealer} : del ps
            | getPlayerRole p1 == Dealer     = del $ ps ++ [p1 {role = None}]
            | otherwise                      = p1 {role = BigBlind} : ps

-- ueberprueft, ob alle wichtigen Rollen (BigBlind, SmallBlind, Dealer) vergeben sind
-- und wenn nicht bekommt ein Spieler die fehlende Rolle
checkSetRoles :: [Player] -> [Player]
checkSetRoles ps = p1 {role = BigBlind} : [p2 {role = SmallBlind}] ++ hilf p
  where hilf [] = [] 
        hilf (p1:ps)
          | length (p1:ps) >= 1 = p1 {role = Dealer} : ps 
          | otherwise = (p1:ps)
        (p1:p2:p) = sort ps

-- Blinds kommen in den Pot; Uebergabeparameter = Small Blind
blinds :: Int -> Int 
blinds v = 3*v 

-- sortiert eine SpielerListe ....,D,S,B zu S,B,....,D
sortBlinds :: [Player] -> [Player]
sortBlinds ps 
  | getPlayerRole (last ps) == BigBlind && length ps > 2 = sortBlinds $ last ps : init ps
  | getPlayerRole (last ps) == SmallBlind && length ps > 2= sortBlinds $ last ps : init ps
  | otherwise = ps

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
completeTableCards deck table = table ++ (head $ austeilen deck 1 [] (5 - length table))

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
runde1b cs ps = (playersWithHands 0,last cards1)
  where cards1 = austeilen cs (length ps) [] 2
        playersWithHands n
            | n < (length ps) = setPlayerHand (cards1 !! n) (ps !! n) : playersWithHands (n+1)
            | otherwise = []

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
  let playersWithCombo = map (getComboForPlayer cs) $ filter getPlayerIngame ps
      winner = playerWithHighestCombo playersWithCombo
      updatedPlayers = payWinner ps (winner,pot)
  printPlayerHands ps []
  putStrLn("Gewonnen hat " ++ (show (map getPlayerName winner)) ++ " mit " ++ show (map getPlayerCombo winner) ++ "")
  putStrLn( show updatedPlayers)
  return updatedPlayers

-- Gewinner bezahlt negativen Betrag = Gewinner bekommt Betrag  
payWinner :: [Player] -> ([Player],Int) -> [Player]
payWinner all (winners,pot) = replace newWinners all
  where newWinners = map (pay (negate $ quot pot (length winners))) winners

-- Gibt den Spieler (bzw. die Spieler) mit der hoechsten Combo aus
-- Funktioniert fuer beliebig viele Spieler
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
        helpReplace [] as vs = as ++ vs
        helpReplace ns [] vs = helpReplace ns vs []
        helpReplace (n:ns) (a:as) vs = if (n==a) then n : helpReplace ns as vs else helpReplace (n:ns) as $ vs ++ [a] 

printPlayerHands :: [Player] -> [ScoreCombo] -> IO ()
printPlayerHands ps cs = do
    let
        printHand p = do
            putStr $ getPlayerName p
            if getPlayerIngame p then do
                putStr " hat auf der Hand "
                putStrLn $ show $ getPlayerHand p
                printPlayerHands (tail ps) (cs ++ [getPlayerCombo p]) 
            else do
                putStrLn " ist ausgestiegen."
                printPlayerHands (tail ps) cs
    if ps==[] then do
        putStrLn ""
    else do
        if cs == [] then do
            printHand $ head ps
        else if all (<= (getPlayerCombo $ head ps)) cs then do 
            printHand $ head ps
        else do
            putStr $ getPlayerName $ head ps
            putStrLn " zeigt seine Karten nicht vor."
            printPlayerHands (tail ps) cs