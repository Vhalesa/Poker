module KI where

import Cards
import Aktionen 
import Combos
import Player
 
-- Abfrage bei der KI: Call, Raise oder Fold?
-- braucht dazu die Player, den Pot und die Tischkarten
-- entscheidungKI :: ([Player],Int) -> Int -> ([Player],Int)
entscheidungKI :: ([Player],Int) -> [Card] -> IO ([Player],Int)
entscheidungKI (p, pot) tisch = do
    putStrLn ""
    putStrLn "KI ist am Zug"
    putStrLn ""
    let
        kiCards :: [Card]
        kiCards = getPlayerHand (head p)

        kiCash :: Int
        kiCash = getPlayerCash (head p)

        kiRaise :: Int
        kiRaise = 200

    --Wenn beide Karten besser als 10, ALL IN
    if (all (>= Card(Spades,Jack)) kiCards) then return $ raise (p, pot) kiCash
    --Wenn eine Karte besser als Dame, RAISE
    else if (any (>= Card(Spades,King)) kiCards && kiCash >= kiRaise) then return $ raise (p, pot) kiRaise
    --Wenn alle Karten schlechter als 6, FOLD
    else if (all (<= Card (Spades,Five)) kiCards) then return $ fold (p, pot)
    --Sonst CALL
    else return $ call (p,pot)
