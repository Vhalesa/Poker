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
    let
        kiCards :: [Card]
        kiCards = getPlayerHand (head p)

        kiCash :: Int
        kiCash = getPlayerCash (head p)

        kiRaise :: Int
        kiRaise = 200

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

    --Wenn beide Karten besser als 10, ALL IN
    if (all (>= Card(Spades,Jack)) kiCards) then do
      putStrLn "KI setzt AllIn ein"
      putStrLn "" 
      return $ raise (p, pot) kiCash
    --Wenn eine Karte besser als Dame, RAISE
    else if (any (>= Card(Spades,King)) kiCards && kiCash >= kiRaise) then do
      putStrLn "KI setzt Raise ein"
      putStrLn ""
      return $ raise (p, pot) kiRaise
    --Wenn alle Karten schlechter als 6, FOLD
    else if (all (<= Card (Spades,Five)) kiCards) then do
      putStrLn "KI setzt Fold ein"
      putStrLn ""
      return $ fold (p, pot)
    --Sonst CALL
    else do
      putStrLn "KI setzt Call ein"
      putStrLn ""
      return $ call (p,pot)



