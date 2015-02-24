module Game where
--bla spiellogik
import Cards
import Chips
import Random
import Combos
import Player

import System.Random

-- Erst Startfunktionen bei Spielstart, dann Endlos weitere Spielrunden, bis Spiel verlassen wird
main = do 
    let player1 = Player { name = "Player 1", hand = [], cash = chips, ki = False, role=None, currentBet=[]}
        player2 = Player { name = "Player 2", hand = [], cash = chips, ki = True, role=None, currentBet=[]}
    deck <- mischen
    print deck
    
       

--alle Methoden, die fuer den Spielablauf benoetigt werden
--
--
--

--Gibt gemischtes Kartendeck zurueck 
mischen = do
  randomNum <- randomIO :: IO Int 
  let generator = makeGenerator randomNum
      mixedDeck = shuffle cards generator []
  return mixedDeck

-- Small und Big Blind zuweisen

-- Runde (ohne Kartenaufdecken)
-- setzen, erhoehen....
runde = undefined

-- Rund1,2,3,4 (jeweils das Karteaufdecken + Aufruf von runde)
-- mit Ausgabe, welche Karte gezogen wurde
--
-- 2 Karten werden fuer jeden Spieler gezogen
--runde1 :: [Card] -> [Player] -> IO
runde1 stapel p = do
  let cards1 = austeilen stapel 2 [] 2  
      p1 = setPlayerHand (head p) (head cards1)
      p2 = setPlayerHand (p !! 1) (cards1 !! 1)
  return ([p1,p2], last cards1)

--runde1 nicht in IO. 
runde1b :: [Card] -> [Player] -> ([Player],[Card])
runde1b cs ps = ([p1,p2],last cards1)
                where cards1 = austeilen cs 2 [] 2  
                      p1 = setPlayerHand (head ps) (head cards1)
                      p2 = setPlayerHand (ps !! 1) (cards1 !! 1)

-- 3 Karten werden vom Stapel genommen
runde2b :: [Card] -> [[Card]]
runde2b cs = austeilen cs 1 [] 3

-- 1 Karte wird vom Stapel genommen (Funktioniert fuer Runde 3 und 4)
runde3b :: [Card] -> [[Card]]
runde3b cs = austeilen cs 1 [] 1
      

-- Zieht n mal jeweils x Karten, und gibt auch den Rest des Decks zurueck [[c1][c2]...[rest]]
-- Kann mit n = 1 genutzt werden, um Karten zum aufdecken zu ziehen
-- kann mit n >= 1 und x = 2 genutzt werden, um Spielern die Startkarten zu ziehen
austeilen :: [Card] -> Int -> [[Card]] -> Int -> [[Card]]
austeilen deck 0 erg x = erg ++ [deck]
austeilen deck n erg x = austeilen (snd $ splitAt x deck) (n-1) (erg ++ [(fst $ splitAt x deck)]) x
     
-- Showdown

-- Entscheidung: weiterspielen oder aufhoeren?

-- Abfrage bei menschlichen Spieler fuer jede Wettrunde/Runde etc.
