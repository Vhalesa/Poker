module KI where

import Cards
import Aktionen 
import Player
import KICalculation

import Data.List
 
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

        kiRaiseBetrag :: Int
        kiRaiseBetrag = if kiCash <= 50 then kiCash else 50 + quot kiCash 15

        kiHandValue :: Int
        kiHandValue = checkKICardValue $ reverse $ sort $ kiCards ++ tisch

        kiToPay :: Int
        kiToPay = maxBet - kiBet

        kiBet :: Int
        kiBet = getCurrentBet $ head p
    
        maxBet :: Int
        maxBet = maximum (map getCurrentBet p)

        kiName :: String
        kiName = getPlayerName $ head p

    putStrLn . show $ kiName
    putStr "Cheatmode: Die Karten der KI sind: "
    putStrLn . show $ kiCards
    putStr "KI hat noch Cash: "
    putStrLn . show $ kiCash 
    putStr "Die höchste Wette ist derzeit bei: "
    putStrLn . show $ maxBet 
    putStr "KIs Wette ist derzeit bei: " -- eher zu Debug-Zwecken (kann nachher evtl weg)
    putStrLn . show $ kiBet 

    -- ALL IN
    if kiHandValue >= 25000 then do
      putStrLn "KI setzt AllIn ein"
      kiRaise (p,pot) kiCash
    -- RAISE
    else if kiHandValue >= 10000 || (tisch == [] && kiHandValue >= 1300) then
      if kiToPay < 300 then
         kiRaise (p,pot) kiRaiseBetrag
      else
         kiCall (p,pot)
    -- CALL
    else if (kiHandValue > 5000 && kiToPay <= kiRaiseBetrag) || (kiHandValue > 800 && tisch == []) || kiToPay <=0 then do
      kiCall (p,pot)
    -- FOLD
    else
      kiFold (p,pot)
      

kiRaise (p, pot) betrag = do
    putStrLn ("KI setzt Raise ein und erhöht um " ++ show betrag)
    putStrLn ""
    return $ raise (p, pot) betrag

kiCall (p,pot) = do
    putStrLn "KI setzt Call ein"
    putStrLn ""
    return $ call (p,pot)

kiFold (p,pot) = do
    putStrLn "KI setzt Fold ein"
    putStrLn ""
    return $ fold (p, pot)
