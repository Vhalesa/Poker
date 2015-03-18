module KI where

import Cards
import Aktionen 
import Player
import KICalculation
import Random

import Data.List
import System.Random
 
-- Abfrage bei der KI: Call, Raise oder Fold?
-- braucht dazu die Player, den Pot und die Tischkarten
-- entscheidungKI :: ([Player],Int) -> Int -> ([Player],Int)
entscheidungKI :: ([Player],Int) -> [Card] -> IO ([Player],Int)
entscheidungKI (p, pot) tisch = do
    putStrLn ""
    let
        kiCards :: [Card]
        kiCards = getPlayerHand (head p)

        kiCash :: Int
        kiCash = getPlayerCash (head p)

        kiMinimumRaise :: Int
        kiMinimumRaise = if kiCash <= 100 then kiCash else 50 + quot kiCash 20

        kiMaximumRaise :: Int
        kiMaximumRaise = min kiCash $ 4*kiMinimumRaise
        
        kiToPay :: Int
        kiToPay = maxBet - kiBet

        kiBet :: Int
        kiBet = getCurrentBet $ head p
    
        maxBet :: Int
        maxBet = maximum (map getCurrentBet p)

        kiName :: String
        kiName = getPlayerName $ head p

        kiRole :: Role
        kiRole = getPlayerRole $ head p

    --muessen in IO, da die Berechnungen nebenlaeufig ueber TVarIO ablaufen
    kiHandValue <- checkKICardValue $ reverse $ sort $ kiCards ++ tisch
    tableValue <- checkKICardValue $ reverse $ sort $ tisch
    kiRaiseBetrag <- kiRandomMoney kiMinimumRaise kiMaximumRaise

    putStr kiName
    putStrLn " ist am Zug"
    putStr kiName
    putStr " ist gerade "
    putStrLn . show $ kiRole
    --putStr "Cheatmode: Die Karten der KI sind: " -- Das hier sollte nach dem Debug auf jeden Fall raus
    --putStrLn . show $ kiCards
    putStr "KI hat noch Cash: "
    putStrLn . show $ kiCash 
    putStr "Im Pot sind zur Zeit: "
    putStrLn . show $ pot
    putStr "Die höchste Wette ist derzeit bei: "
    putStrLn . show $ maxBet 
    putStr "KIs Wette ist derzeit bei: " -- eher zu Debug-Zwecken (kann nachher evtl weg)
    putStrLn . show $ kiBet 

    -- ALL IN
    -- KI hat einen guten Flush oder besseres
    if kiHandValue >= 28000 then do
      putStrLn "KI setzt AllIn ein"
      kiRaise (p,pot) kiCash

    -- RAISE
    --      KI hat min. 2Pair       KI hat gute Handkarten                  KI hat min. ein Pair und es liegen schon 4 Karten und der Pot ist klein
    else if kiHandValue >= 10000 || (tisch == [] && kiHandValue >= 1300) || (length tisch >= 4 && kiHandValue - tableValue > 5000 && pot <= quot kiCash 15) then
      if kiToPay <= 4*kiMinimumRaise then
         kiRaise (p,pot) $ max kiRaiseBetrag kiToPay
      else
         kiCall (p,pot)

    -- CALL
    --      KI hat min Pair und muss nicht zu viel zahlen                       KI hat okay Handkarten und muss nicht zu viel zahlen                 KI hat 10- Chips KI muss nix zahlen
    else if (kiHandValue - tableValue > 5000 && kiToPay <= 2*kiMinimumRaise) || (kiHandValue > 800 && tisch == [] && kiToPay <= 2*kiMinimumRaise) || kiCash <= 10 || kiToPay <=0 then do
      kiCall (p,pot)
    -- FOLD
    else
      kiFold (p,pot)
      

kiRaise (p, pot) betrag = do
    putStr $ show $ getPlayerName (head p)
    putStr " setzt Raise ein und erhoeht um "
    putStrLn $ show betrag
    putStrLn ""
    return $ raise (p, pot) betrag

kiCall (p,pot) = do
    putStr $ show $ getPlayerName (head p)
    putStrLn " setzt Call ein"
    putStrLn ""
    return $ call (p,pot)

kiFold (p,pot) = do
    putStr $ show $ getPlayerName (head p)
    putStrLn " setzt Fold ein"
    putStrLn ""
    return $ fold (p, pot)

-- gibt einen Random Int zwischen l und r zurueck
kiRandomMoney :: Int -> Int -> IO Int
kiRandomMoney l r = do
  randomNum <- randomIO :: IO Int 
  let generator = makeGenerator randomNum
      randomMoney = randomBetrag generator l r
  return randomMoney
