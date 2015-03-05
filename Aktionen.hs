module Aktionen where

import Player
import Cards

--p1 folded
fold :: ([Player],Int) -> ([Player],Int)
fold ((p1:ps),pot) = (ps++[setPlayerIngame False p1],pot)

-- p1 erhoeht 
-- bekommt die Liste der Player und den Pot (und gibt diese mit Veraenderung wieder zurueck)
call :: ([Player],Int) -> ([Player],Int)
call p = raise p 0 

-- p1 erhoeht um betrag 
-- bekommt die Liste der Player, den Pot und den erhoehten Betrag 
-- Reihenfolge der Spieler wird gleich um 1 verschoben -> naechster Spieler ist dran
raise :: ([Player],Int) -> Int -> ([Player],Int)
raise ((p1:p2:ps),pot) betrag = if ((getPlayerCash p1 - diff) >= 0)  
                                  then (p2:ps ++ [pay diff p1], pot + diff)
                                  else (newPlayer2:ps ++ [pay allIn p1], (pot + allIn - (diff2 - allIn)))
  where diff = ((maximum (map getCurrentBet (p1:p2:ps))) - getCurrentBet p1) + betrag
        diff2 = ((maximum (map getCurrentBet (p1:p2:ps))) - getCurrentBet p1) 
        allIn = getPlayerCash p1
        newPlayer2 = pay (allIn - diff2) p2 --Player2 bekommt sein ueberschuessiges Geld zurueck

-- Spieler bezahlt aus seinem Geld einen bestimmten Betrag
pay :: Int -> Player -> Player
pay 0 p = p 
pay x p = p {cash = getPlayerCash p - x, currentBet = getCurrentBet p + x}

