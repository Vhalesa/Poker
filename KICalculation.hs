module KICalculation where

import Combos
import Cards


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
