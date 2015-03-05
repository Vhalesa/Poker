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
        kiToPay = maxBet - kiBet

        kiBet :: Int
        kiBet = getCurrentBet $ head p
    
        maxBet :: Int
        maxBet = getCurrentBet $ p !! 1

    putStr "KI hat noch Cash: "
    putStrLn . show $ kiCash 
    putStr "Die höchste Wette ist derzeit bei: "
    putStrLn . show $ maxBet 
    putStr "KIs Wette ist derzeit bei: " -- eher zu Debug-Zwecken (kann nachher evtl weg)
    putStrLn . show $ kiBet 

    -- ALL IN
    if kiHandValue >= 2500 then do
      putStrLn "KI setzt AllIn ein"
      putStrLn "" 
      return $ raise (p, pot) kiCash
    -- RAISE
    else if kiHandValue >= 1300 && kiToPay < 200 then do
      putStrLn "KI setzt Raise ein"
      putStrLn ""
      return $ raise (p, pot) kiRaise
    -- FOLD
    else if kiHandValue < 800 && kiToPay > 0 then do
      putStrLn "KI setzt Fold ein"
      putStrLn ""
      return $ fold (p, pot)
    -- CALL
    else do
      putStrLn "KI setzt Call ein"
      putStrLn ""
      return $ call (p,pot)
