#!/usr/bin/runhaskell
--Dieses Modul kuemmert sich um den Ablauf des Spiels. Hier werden Karten gegeben, Einsaetze gemacht usw.
module Game where


import Cards
--import Chips
import Random
import Combos
import Player
import Mensch
import Aktionen
import KI

import System.Random
import Data.List
import Control.Monad

-- Erst Startfunktionen bei Spielstart, dann Endlos weitere Spielrunden, bis Spiel verlassen wird
main = do 
    let player1 = Player { name = "Player 1", hand = [], combo = HighCard [], cash = 4000, ki = False, role=BigBlind, ingame = True, currentBet=0}
        player2 = Player { name = "Awesome KI", hand = [], combo = HighCard [], cash = 4000, ki = True, role=SmallBlind, ingame = True, currentBet=0}
        player3 = Player { name = "Die Andere KI", hand = [], combo = HighCard [], cash = 4000, ki = True, role=None, ingame = True, currentBet=0}
    startGame [player1,player2] 1

--alle Methoden, die fuer den Spielablauf benoetigt werden

--Ein komplettes Spiel mit einer Liste an Spielern durchfuehren; n = n-te Spielrunde
startGame ps n = do
    --Karten mischen
    deck <- mischen
    --print deck

    putStrLn ""
    putStrLn "  ██████╗  ██████╗ ██╗  ██╗███████╗██████╗"
    putStrLn "  ██╔══██╗██╔═══██╗██║ ██╔╝██╔════╝██╔══██╗"
    putStrLn "  ██████╔╝██║   ██║█████╔╝ █████╗  ██████╔╝"    
    putStrLn "  ██╔═══╝ ██║   ██║██╔═██╗ ██╔══╝  ██╔══██╗"    
    putStrLn "  ██║     ╚██████╔╝██║  ██╗███████╗██║  ██║"    
    putStrLn "  ╚═╝      ╚═════╝ ╚═╝  ╚═╝╚══════╝╚═╝  ╚═╝"    
                                             
    putStrLn ""
    putStr "         "
    putStrLn "\ESC[5;32m$$$\ESC[0m \ESC[5mWIN BIG MONEY \ESC[5;32m$$$\ESC[0m"
    putStrLn ""
    putStrLn ""
  
    --Blinds bezahlen 
    let blindsMultiplikator = 1 + (quot (n-1) $ length ps)
    playersAndPot <- doBlinds ps (blindsMultiplikator*10)
    
    --Runde 1 ausfuehren: Karten austeilen und Spieler duerfen setzen,...
    --Karten austeilen 
    let r1 = runde1b deck $ fst playersAndPot
        players1 = fst $ r1
        deck1 = snd $ r1

    -- Setzen -> Ueberpruefen, ob es schon einen Gewinner gibt?
    playersAndPot1 <- runde (players1, snd playersAndPot) []
    if ( not $ all getPlayerIngame $ tail (fst $ playersAndPot1))
      then do
        continueGame (payWinner (fst playersAndPot1) ([head $ fst playersAndPot1],snd playersAndPot1)) n
    else if ( any (<= 0) $ map getPlayerCash $ fst playersAndPot1)
      then do
        --Showdown, da ein Spieler AllIn eingesetzt hat und kein Geld mehr hat
        playersAfterShowdown <- showdown playersAndPot1 $ completeTableCards deck1 [] 
        --Weiterspielen?
        continueGame playersAfterShowdown n
    else do 
      --Runde 2 ausfuehren: 3 Karten als Flop austeilen und wieder setzen
      r2 <- runde2 deck1
      let deck2 = last r2

      --Setzen
      playersAndPot2 <- runde playersAndPot1 (head r2)
      if ( not $ all getPlayerIngame $ tail (fst $ playersAndPot2))
        then do
          continueGame (payWinner (fst playersAndPot2) ([head $ fst playersAndPot2],snd playersAndPot2)) n
      else if ( any (<= 0) $ map getPlayerCash $ fst playersAndPot2)
        then do
          --Showdown, da ein Spieler AllIn eingesetzt hat und kein Geld mehr hat
          playersAfterShowdown <- showdown playersAndPot2 $ completeTableCards deck2 (head r2)
          --Weiterspielen?
          continueGame playersAfterShowdown n
      else do
        --Runde 3 ausfuehren: 1 Karte als Turn austeilen und wieder setzen
        r3 <- runde3 deck2
        let deck3 = last r3
            tischkarten = head r2 ++ head r3

        --Setzen 
        playersAndPot3 <- runde playersAndPot2 tischkarten
        if ( not $ all getPlayerIngame $ tail (fst $ playersAndPot3))
          then do continueGame (payWinner (fst playersAndPot3) ([head $ fst playersAndPot3],snd playersAndPot3)) n
        else if ( any (<= 0) $ map getPlayerCash $ fst playersAndPot3)
          then do
            --Showdown, da ein Spieler AllIn eingesetzt hat und kein Geld mehr hat
            playersAfterShowdown <- showdown playersAndPot3 $ completeTableCards deck3 tischkarten 
            --Weiterspielen?
            continueGame playersAfterShowdown n
        else do
          --Runde 4 ausfuehren: 1 Karte als River austeilen und wieder setzen
          r4 <- runde4 deck3
          let finalTischkarten = tischkarten ++ head r4

          --Setzen
          playersAndPot4 <- runde playersAndPot3 finalTischkarten
         -- if ( not $ all getPlayerIngame $ tail (fst $ playersAndPot4)) 
         --   then do
         --     continueGame (payWinner (fst playersAndPot4) ([head $ fst playersAndPot4],snd playersAndPot4)) n
         --   else do
         --     --Showdown, wer hat gewonnen??
         --     playersAfterShowdown <- showdown (playersAndPot4) (finalTischkarten)
         --     --Weiterspielen?
         --     continueGame playersAfterShowdown n
         --
          allIngame4 <- checkAllInGame playersAndPot4 n 
          when allIngame4 $ do
              --Showdown, wer hat gewonnen??
              playersAfterShowdown <- showdown (playersAndPot4) (finalTischkarten)
              --Weiterspielen?
              continueGame playersAfterShowdown n

-- ueberprueft, ob noch alle Spieler im Spiel sind, oder es schon einen Gewinner gibt
--checkAllInGame :: ([Player],Int) -> IO ()
checkAllInGame playersAndPot n = do
  if ( not $ all getPlayerIngame $ tail (fst $ playersAndPot)) 
    then do
      continueGame (payWinner (fst playersAndPot) ([head $ fst playersAndPot],snd playersAndPot)) n
      return False 
    else
      return True 

--Gibt gemischtes Kartendeck zurueck 
mischen = do
  randomNum <- randomIO :: IO Int 
  let generator = makeGenerator randomNum
      mixedDeck = shuffle cards generator []
  return mixedDeck

-- Runde ohne Kartenaufdecken, das wurde davor schon gemacht
-- setzen, erhoehen....
-- es muessen die [Player], der Pot und die Tischkarten uebergeben werden
runde :: ([Player],Int) -> [Card]-> IO ([Player],Int)
runde (p, pot) tisch = do
  x <- rundeImmer (p,pot) tisch --Small Blind ist immer dran
  y <- rundeImmer x tisch --Big Blind ist immer dran
  wdhRunde y tisch --solange, bis entweder alle Spieler die gleiche Wette haben, oder nur noch 1 im Spiel ist
    where rundeImmer :: ([Player],Int) -> [Card] -> IO ([Player],Int)
          rundeImmer ((p1:ps),pot) tisch  
              -- Spieler, der dran ist, ist nicht mehr im Spiel (wegen Fold) -> naechster Spieler ist dran
              | (not $ getPlayerIngame p1)             = return $ nextPlayer ((p1:ps),pot)
              -- wenn nur noch ein Spieler im Spiel ist -> jetzigen Wert zurueckgeben (wenn es nur 2 Spieler gibt
              -- darf der 2. nicht noch setzen, wenn 1. Fold eingesetzt hat 
              | all (==False) $ map getPlayerIngame ps = return $ ((p1:ps),pot)
              | getKI p1                               = entscheidungKI ((p1:ps),pot) tisch
              | otherwise                              = entscheidungMensch ((p1:ps),pot) tisch
          wdhRunde :: ([Player],Int) -> [Card] -> IO ([Player],Int)
          wdhRunde ((p1:p2:ps),pot) tisch
            -- alle Spieler haben den gleichen Wettbetrag -> Setzrunde vorbei
            | all (==True) $ map ((getCurrentBet p1) ==) $ map getCurrentBet (p2:ps) = return ((p1:p2:ps),pot)
            -- nur noch ein Spieler im Spiel -> beende
            | (getPlayerIngame p1) && (all (==False) $ map getPlayerIngame (p2:ps)) = return ((p1:p2:ps),pot)
            --sonst: naechster Spieler ist dran
            | otherwise = (rundeImmer ((p1:p2:ps),pot) tisch) >>= (\x -> wdhRunde x tisch)
          --naechster Player kommt an Anfang der Liste (1.Player an den Schluss)  
          nextPlayer :: ([Player],Int) -> ([Player],Int)
          nextPlayer ((p1:ps),pot) = ((ps ++ [p1]),pot)

-- Entscheidung: weiterspielen oder aufhoeren?
continueGame :: [Player] -> Int -> IO ()
continueGame ps n = do
    putStrLn ("Die " ++ show n ++ ". Spielrunde ist vorbei. Weiterspielen? (Y/N)")
    input <- getLine
    if (any (<=10) $ map getPlayerCash ps) 
      then do
        putStrLn "Mindestens ein Spieler hat zu wenig Geld. Das Spiel ist deshalb beendet."
        --todo falls wir mehr als 2 Spieler machen: Ohne den Spieler weiterspielen?
    else if (input=="y" || input=="Y") 
      then do
        putStrLn "Die naechste Spielrunde beginnt gleich!"
        let updatedPlayers = resetIngame $ resetCombos $ resetHands $ resetBets ps
        startGame updatedPlayers $ n+1
    else if (input=="n" || input=="N") 
      then do
        putStrLn "Du hast das Spiel beendet."
    else do
        continueGame ps n

