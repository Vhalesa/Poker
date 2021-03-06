module Mensch where

import Cards
import Player
import Aktionen

import Control.Monad

-- Abfrage beim Mensch: Call, Raise oder Fold?
-- braucht dazu die Player, den Pot und die Tischkarten
entscheidungMensch :: ([Player],Int) -> [Card] -> IO ([Player],Int)
entscheidungMensch (p, pot) tisch = do
  putStrLn ""
  putStr $ getPlayerName $ head p
  putStrLn ", du bist dran."
  abfrage
  where abfrage = do
          let hand = getPlayerHand $ head p
          putStr "Du hast folgende Handkarten: "
          putStr .  show $ head hand
          putStr " und "
          putStr .  show $ hand !! 1
          putStrLn ""
          --auf dem Tisch liegen die Karten...
          when (not $ null tisch) $ do
            putStr "Auf dem Tisch liegen: "
            (putStr . show) tisch
            putStrLn ""
          --du hast noch xxx Geld und weitere Info Ausgaben
          let eigCash = getPlayerCash $ head p
              bet = maximum (map getCurrentBet p)
              eigBet = getCurrentBet $ head p
              allIn = eigCash - (bet - eigBet) 
              eigRole = getPlayerRole $ head p
          putStr "Du bist "
          putStrLn . show $ eigRole 
          putStr "Im Pot sind zur Zeit: "
          putStrLn . show $ pot
          putStr "Du hast noch Cash: "
          putStrLn . show $ eigCash 
          putStr "Die höchste Wette ist derzeit bei: "
          putStrLn . show $ bet
          putStr "Deine Wette ist derzeit bei: " 
          putStrLn . show $ eigBet
          putStrLn "Was möchtest du tun? (Call/Raise/Fold)"
          input <- getLine
          let inputSplit = words input
          if (input == "")
            then do
              putStrLn "Du musst schon was eingeben! (Call/Raise/Fold)"
              abfrage
          else if (input == "Call" || input == "call")
            then do
              putStrLn "Du hast Call eingesetzt. It's very effektive"
              return $ call (p, pot)
          else if (input  == "Fold" || input == "fold")
             then do
              putStrLn "Du hast Fold eingesetzt. It's not very effektive"
              return $ fold (p,pot)
          else if (input  == "Raise" || input == "raise")
             then do
              putStrLn "Du hast Raise eingesetzt. Um wie viel möchtest du erhöhen?"
              raiseAbfrage allIn
          else if ((head inputSplit == "Raise" || head inputSplit == "raise") && (length inputSplit) == 2)
            then do
              checkRaiseAbfrage (inputSplit !! 1) allIn
          else do
            putStrLn "Du musst Call, Raise oder Fold eingeben."
            abfrage
        -- Abfrage des Betrags, um den der Spieler erhoehen will
        raiseAbfrage allIn = do
          eingabe <- getLine
          checkRaiseAbfrage eingabe allIn
        checkRaiseAbfrage eingabe allIn = do
          if (null (isInt eingabe)) || ( (snd $ head $ isInt eingabe) /= "")
            then do
              putStr "Du musst eine ganze Zahl eingeben, zwischen 0 und "
              putStrLn . show $ allIn
              raiseAbfrage allIn
            else do
              let betrag = fst $ head (isInt eingabe)
              if (betrag < 0)
                then do
                  putStrLn "Du kannst nicht um einen negativen Betrag erhöhen. Du Cheater!"
                  putStrLn "Gib gefälligst eine positive Zahl ein."
                  raiseAbfrage allIn
                else if (betrag == 0)
                  then do
                    putStrLn "Mensch, da hättest du auch Call nehmen können -.-"
                    return $ call (p,pot)
                else if (betrag > allIn)
                  then do
                    putStrLn "Du kannst nicht um mehr erhöhen als du Geld hast!"
                    putStr "Du musst eine ganze Zahl eingeben, zwischen 0 und "
                    putStrLn . show $ allIn
                    raiseAbfrage allIn
                else do
                  putStrLn ("Du hast um den Betrag " ++ eingabe ++ " erhöht.")
                  return $ raise (p,pot) betrag

--gibt wenn Anfang ein Int einen [(Int,RestString)] zurueck. Ansonsten eine leere Liste
isInt :: String -> [(Int,String)]
isInt x = reads x

--fragt beim menschlichen Spieler ab, gegen wie viele KIs er spielen will
--zur Zeit 1-7 moeglich
anzahlPlayer :: IO Int
anzahlPlayer = do
  putStrLn "Mit wie vielen KIs möchtest du spielen? Du kannst gegen 1 bis 7 KIs spielen." 
  abfrage
  where abfrage = do
          eingabe <- getLine
          if (null (isInt eingabe)) || ( (snd $ head $ isInt eingabe) /= "")
            then do
              putStrLn "Du musst eine Zahl zwischen 1 und 7 eingeben!"
              abfrage
            else do
              let betrag = fst $ head (isInt eingabe)
              if (betrag > 0 && betrag < 8)
                then do return $ betrag + 1
              else do
                putStrLn "Momentan kann man nur gegen 1 bis 7 Gegener spielen."
                abfrage
