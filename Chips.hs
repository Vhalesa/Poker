module Chips where

data Chip = Chip {value :: Int}
        deriving (Eq, Ord)

instance Show Chip where
        show (Chip v) = show v

--Anfangschips fuer jeden Spieler im Gesamtwert 4000
chips :: [Chip]
chips = [Chip 5,Chip 5,Chip 5,Chip 5,Chip 5,Chip 5,Chip 5,Chip 5,Chip 5,Chip 5,Chip 5,Chip 5,Chip 5,Chip 5,Chip 5,Chip 5, -- 16x5 = 80
         Chip 10, Chip 10, Chip 10, Chip 10, Chip 10, Chip 10, Chip 10, Chip 10, Chip 10, Chip 10,
         Chip 10, Chip 10, Chip 10, Chip 10, Chip 10, Chip 10, Chip 10, Chip 10, Chip 10, Chip 10, -- 20x10 = 200
         Chip 20, Chip 20, Chip 20, Chip 20, Chip 20, Chip 20, Chip 20, Chip 20, Chip 20, Chip 20, Chip 20, --11x20 = 220
         Chip 50, Chip 50, Chip 50, Chip 50, Chip 50, Chip 50, Chip 50, Chip 50, Chip 50, Chip 50, --10x50 = 500
         Chip 100, Chip 100, Chip 100, Chip 100, Chip 100, Chip 100, Chip 100, Chip 100,-- 8x100 = 800
         Chip 200, Chip 200, Chip 200, Chip 200, Chip 200, Chip 200, -- 6x200 = 1200
         Chip 500, Chip 500] --2x500 = 1000

--Wert einer Summe von Chips
sum :: [Chip] -> Int
sum [] = 0
sum ((Chip v):t) = v + Chips.sum t

change :: Chip -> [Chip]
change (Chip 500) = [Chip 200, Chip 100, Chip 100, Chip 50, Chip 50]
change (Chip 200) = [Chip 100, Chip 50, Chip 20, Chip 20, Chip 10]
change (Chip 100) = [Chip 50, Chip 20, Chip 20, Chip 10]
change (Chip 50) = [Chip 20, Chip 10, Chip 10, Chip 10]
change (Chip 20) = [Chip 10, Chip 10]
change (Chip 10) = [Chip 5, Chip 5]
change c = [c]