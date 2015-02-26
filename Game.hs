module Game where
--bla spiellogik
import Cards
--import Chips
import Random
import Combos
import Player

import System.Random
import Data.List

-- Erst Startfunktionen bei Spielstart, dann Endlos weitere Spielrunden, bis Spiel verlassen wird
main = do 
    let player1 = Player { name = "Player 1", hand = [], combo = HighCard [], cash = 4000, ki = False, role=BigBlind, currentBet=0}
        player2 = Player { name = "Player 2", hand = [], combo = HighCard [], cash = 4000, ki = True, role=SmallBlind, currentBet=0}
    startGame [player1,player2]

--alle Methoden, die fuer den Spielablauf benoetigt werden

--Eine komplette Spielrunde mit einer Liste an Spielern durchfuehren
startGame ps = do
    --Karten mischen
    deck <- mischen
    print deck

    --Blinds bezahlen
    playersAndPot <- doBlinds ps 10
    
    --Runde 1 ausfuehren: Karten austeilen und Spieler duerfen setzen,...
    --ToDo

    --Runde 2 ausfuehren: 3 Karten als Flop austeilen und wieder setzen
    --ToDo

    --Runde 3 ausfuehren: 1 Karte als Turn austeilen und wieder setzen
    --ToDo 

    --Runde 4 ausfuehren: 1 Karte als River austeilen und wieder setzen
    --ToDo

    --Showdown, wer hat gewonnen??
    --ToDo

    --Weiterspielen?
    --ToDo

    
    
    putStrLn (show (runde1bis4 deck)) --Platzhalter, damit es ausfuehrbar ist. Kann spaeter weg!

--Gibt gemischtes Kartendeck zurueck 
mischen = do
  randomNum <- randomIO :: IO Int 
  let generator = makeGenerator randomNum
      mixedDeck = shuffle cards generator []
  return mixedDeck

--Kuemmert sich um alles, was die Blinds angeht, Blinds zuweisen, bezahlen und in den Pot packen.
doBlinds :: [Player] -> Int -> IO([Player],Int)
doBlinds ps x = do
    let ps1 = map delegateBlind ps
        ps2 = map (payBlind x) ps1
        pot = blinds x
    putStrLn ("Der Pot betraegt nach den Blinds " ++ show pot)
    putStrLn ("Spieler 1 hat noch " ++ show (getPlayerCash $ head ps2) ++ " Chips")
    putStrLn ("Spieler 2 hat noch " ++ show (getPlayerCash $ last ps2) ++ " Chips")
    return (ps2,pot)
        

-- Small und Big Blind zuweisen
delegateBlind :: Player -> Player
delegateBlind p
    | getPlayerRole p == BigBlind = p {role = SmallBlind}
    | getPlayerRole p == SmallBlind = p {role = BigBlind}
    | otherwise = p

-- Blinds kommen in den Pot; Uebergabeparameter = Small Blind
blinds :: Int -> Int
blinds v = 3*v

-- Spieler muss Blind bezahlen Uebergabeparameter = Small Blind
payBlind :: Int -> Player -> Player
payBlind v p
    | getPlayerRole p == BigBlind = pay p (2*v)
    | getPlayerRole p == SmallBlind = pay p v
    | otherwise = p


-- p1 erhoeht 
-- bekommt die Liste der Player und den Pot (und gibt diese mit Veraenderung wieder zurueck)
call :: ([Player],Int) -> ([Player],Int)
call p = raise p 0

-- p1 erhoeht um betrag 
-- bekommt die Liste der Player, den Pot und den erhoehten Betrag 
raise :: ([Player],Int) -> Int -> ([Player],Int)
raise ((p1:p2:ps),pot) betrag = (p2:ps ++ [pay p1 diff], pot + diff)
  where diff = getCurrentBet p2 - getCurrentBet p1 + betrag


-- Spieler bezahlt aus seinem Geld einen bestimmten Betrag
pay :: Player -> Int -> Player
pay p 0 = p
pay p x = p {cash = (getPlayerCash p)-x, currentBet = (getCurrentBet p) + x}
    
-- Der CurrentBet aller Spieler wird wieder auf 0 gesetzt (vor jeder Setzrunde erforderlich)
resetBets :: [Player] -> [Player]
resetBets ps = map removeCurrentBet ps

-- Runde (ohne Kartenaufdecken)
-- setzen, erhoehen....


-- Rund1,2,3,4 (jeweils das Karteaufdecken + Aufruf von runde)
-- mit Ausgabe, welche Karte gezogen wurde
--
-- 2 Karten werden fuer jeden Spieler gezogen
--runde1 :: [Card] -> [Player] -> IO
runde1 stapel p = do
  let cards1 = austeilen stapel 2 [] 2  
      p1 = setPlayerHand (head cards1) (head p) 
      p2 = setPlayerHand (cards1 !! 1) (p !! 1)
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
runde3b :: [Card] -> [[Card]]
runde3b cs = austeilen cs 1 [] 1
--Testfunktion um 5 random Karten zu bekommen. Kann danach vermutlich wieder geloescht werden!!
runde1bis4 :: [Card] -> [Card]
runde1bis4 cs = f3 ++ s1 ++ t1 
    where
        f3 = head f
        s1 = head s
        t1 = head t
        f = runde2b cs
        s = runde3b $ last f
        t = runde3b $ last s

-- Zieht n mal jeweils x Karten, und gibt auch den Rest des Decks zurueck [[c1][c2]...[rest]]
-- Kann mit n = 1 genutzt werden, um Karten zum aufdecken zu ziehen
-- kann mit n >= 1 und x = 2 genutzt werden, um Spielern die Startkarten zu ziehen
austeilen :: [Card] -> Int -> [[Card]] -> Int -> [[Card]]
austeilen deck 0 erg x = erg ++ [deck]
austeilen deck n erg x = austeilen (snd $ splitAt x deck) (n-1) (erg ++ [(fst $ splitAt x deck)]) x
     
-- Showdown
--showdown :: ([Player],Int) -> IO
showdown (ps,pot) cs = do
    let
        playerCardSet = map (getComboForPlayer cs) ps
    winner <- undefined
    putStrLn("Gewonnen hat ")

-- Ermittelt fuer einen Spieler anhand der uebergebenen (Tisch-)Karten die Combo fuer den Spieler
-- und traegt diese im Spieler ein.
getComboForPlayer :: [Card] -> Player -> Player
getComboForPlayer cs p = setPlayerCombo (checkCombo (reverse $ sort $ getPlayerHand p ++ cs)) p

-- Entscheidung: weiterspielen oder aufhoeren?

-- Abfrage bei menschlichen Spieler fuer jede Wettrunde/Runde etc.
