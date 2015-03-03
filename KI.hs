module KI where

import Cards
import Game
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
        
    if (any (>= Card(Spades,Jack)) kiCards) then return $ raise (p, pot) 200
    else if (all (<= Card (Spades,Five)) kiCards) then return $ fold (p, pot)
    else return $ call (p,pot)