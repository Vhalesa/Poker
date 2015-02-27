--Dieses Modul war urspruenglich dafuer gedacht, die Chips zu verwalten. Es wird derzeit nicht benutzt.
module Chips where

data Chip = Chip {value :: Int}
        deriving (Eq, Ord)

instance Show Chip where
        show (Chip v) = show v

--Anfangschips fuer jeden Spieler im Gesamtwert 4000
chips :: [Chip]
chips = a ++ b ++ c ++ d ++ e ++ f ++ g
  where a = replicate 16 (Chip 5)  --16x5  = 80
        b = replicate 20 (Chip 10) --20x10 = 200
        c = replicate 11 (Chip 20) --11x20 = 220
        d = replicate 10 (Chip 50) --10x50 = 500
        e = replicate 8 (Chip 100) --8x100 = 800
        f = replicate 6 (Chip 200) --6x200 = 1200
        g = replicate 2 (Chip 500) --2x500 = 1000

--Wert einer Summe von Chips
sum :: [Chip] -> Int
sum [] = 0
sum ((Chip v):t) = v + Chips.sum t

-- Wechselt einen Chip in eine kleinere Menge Chips und gibt die kleinere Menge zurueck
change :: Chip -> [Chip]
change (Chip 500) = [Chip 200, Chip 100, Chip 100, Chip 50, Chip 50]
change (Chip 200) = [Chip 100, Chip 50, Chip 20, Chip 20, Chip 10]
change (Chip 100) = [Chip 50, Chip 20, Chip 20, Chip 10]
change (Chip 50) = replicate 3 (Chip 10) ++ [Chip 20]
change (Chip 20) = replicate 2 (Chip 10)
change (Chip 10) = replicate 2 (Chip 5)
change c = [c]

-- Methode, Chips aus der Hand in den Pot zu legen. Der abgelegte Chipsstapel wird zurueckgegeben
-- Ueberpruefung, ob Spieler genug Chips hat NICHT hier
putInPot :: Int -> [Chip] -> [Chip]
putInPot 0 _ = []
putInPot v [] = [Chip v] -- Schoener waere: Chips wechseln!!
putInPot v (c:cs) = if (Chips.sum[c] <= v) then c: putInPot (v - (Chips.sum [c])) cs else putInPot v cs

--Methode, analog zu putinPot, nur dass die behaltenen Chips ausgegeben werden
keepInHand :: Int -> [Chip] -> [Chip]
keepInHand 0 cs = cs
keepInHand v (c:cs) = if (Chips.sum[c] <= v) then keepInHand (v - (Chips.sum [c])) cs else c : keepInHand v cs