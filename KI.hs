module KI where

import Cards
import Aktionen 
import Player
import KICalculation
 
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

        kiHandValue :: Int
        kiHandValue = handValue kiCards

        kiToPay :: Int
        kiToPay = quot pot 2 - getCurrentBet (head p)

    -- ALL IN
    if kiHandValue > 2500 then return $ raise (p, pot) kiCash
    -- RAISE
    else if kiHandValue > 1600 && kiToPay < 200 then return $ raise (p, pot) kiRaise
    -- FOLD
    else if kiHandValue < 800 && kiToPay >= 0 then return $ fold (p, pot)
    -- CALL
    else return $ call (p,pot)
