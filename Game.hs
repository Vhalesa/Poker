module Game where
--bla spiellogik
import Cards
import Chips
import Random
import Combos

import System.Random

-- Erst Startfunktionen bei Spielstart, dann Endlos weitere Spielrunden, bis Spiel verlassen wird
main = undefined

--alle Methoden, die fuer den Spielablauf benoetigt werden

--Gibt gemischtes Kartendeck aus (im Moment auf die Kommandozeile)
mischen = do
  randomNum <- randomIO :: IO Int 
  let generator = makeGenerator randomNum
  let mixedDeck = shuffle cards generator []
  print mixedDeck

-- Chipshaufen zuweisen?

-- Small und Big Blind zuweisen

-- Runde (ohne Kartenaufdecken)
-- setzen, erhoehen....
runde = undefined

-- Rund1,2,3,4 (jeweils das Karteaufdecken + Aufruf von runde)
-- mit Ausgabe, welche Karte gezogen wurde

-- Showdown

-- Entscheidung: weiterspielen oder aufhoeren?

-- Abfrage bei menschlichen Spieler fuer jede Wettrunde/Runde etc.
