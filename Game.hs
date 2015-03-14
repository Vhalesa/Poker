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
    let player1 = Player { name = "Human Genius", hand = [], combo = HighCard [], cash = 4000, ki = False, role=BigBlind, ingame = True, currentBet=0}
        player2 = Player { name = "Awesome KI", hand = [], combo = HighCard [], cash = 4000, ki = True, role=SmallBlind, ingame = True, currentBet=0}
        player3 = Player { name = "Majestic KI", hand = [], combo = HighCard [], cash = 4000, ki = True, role=None, ingame = True, currentBet=0}
        player4 = Player { name = "Superb KI", hand = [], combo = HighCard [], cash = 4000, ki = True, role=Dealer, ingame = True, currentBet=0}

        players = [player3,player4,player2,player1]

    -- Mensch waehlt aus, gegen wie viele KIs er spielen moechte
    anzahl <- anzahlPlayer
    let playersWahl = fst $ splitAt anzahl $ reverse players 
    startGame playersWahl 1

--alle Methoden, die fuer den Spielablauf benoetigt werden

--Ein komplettes Spiel mit einer Liste an Spielern durchfuehren; n = n-te Spielrunde
startGame ps n = do
    --Karten mischen
    deck <- mischen

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

    -- Pruefen, ob noch alle wichtigen Rollen dabei sind, und ansonsten neu verteilen
    let psCheck = checkSetRoles ps
    
    --Blinds bezahlen 
    let blindsMultiplikator = 1 + (quot (n-1) $ length psCheck)
    playersAndPot <- doBlinds psCheck (blindsMultiplikator*10)
    
    --Runde 1 ausfuehren: Karten austeilen und Spieler duerfen setzen,...
    --Karten austeilen 
    let r1 = runde1b deck $ fst playersAndPot
        players1 = fst $ r1
        deck1 = snd $ r1

    -- Reihefolge: ...,D,S,B
    -- Setzen -> Ueberpruefen, ob es schon einen Gewinner gibt?
    playersAndPot1 <- runde (players1, snd playersAndPot) []
    allIngame1 <- checkAllInGame playersAndPot1 n deck1 [] 
    when allIngame1 $ do

      --Runde 2 ausfuehren: 3 Karten als Flop austeilen und wieder setzen
      r2 <- runde2 deck1
      let deck2 = last r2
          tischkarten2 = head r2

      -- TODO: Liste der Player so sortieren: S,B,...,D
      let playersAndPot1s = (sortBlinds $ fst playersAndPot1, snd playersAndPot1)
      print $ fst playersAndPot1s
      
      --Setzen fuer Runde 2
      playersAndPot2 <- runde playersAndPot1s (head r2)
      allIngame2 <- checkAllInGame playersAndPot2 n deck2 tischkarten2
      when allIngame2 $ do

        --Runde 3 ausfuehren: 1 Karte als Turn austeilen und wieder setzen
        r3 <- runde3 deck2
        let deck3 = last r3
            tischkarten = tischkarten2 ++ head r3

        --Setzen fuer Runde 3
        playersAndPot3 <- runde playersAndPot2 tischkarten
        allIngame3 <- checkAllInGame playersAndPot3 n deck3 tischkarten
        when allIngame3 $ do

          --Runde 4 ausfuehren: 1 Karte als River austeilen und wieder setzen
          r4 <- runde4 deck3
          let finalTischkarten = tischkarten ++ head r4
              deck4 = last r4

          --Setzen fur Runde 4
          playersAndPot4 <- runde playersAndPot3 finalTischkarten
          allIngame4 <- checkAllInGame playersAndPot4 n deck3 finalTischkarten
          when allIngame4 $ do
            -- Showdown
            endgame playersAndPot4 finalTischkarten n

--ueberprueft, ob noch alle Spieler im Spiel sind, oder es schon einen Gewinner gibt
--checkAllInGame :: ([Player],Int) -> Int -> IO Bool
checkAllInGame playersAndPot n deck tisch
  -- nur noch ein Spieler dabei -> es wird beendet
  | length (filter getPlayerIngame (fst playersAndPot)) == 1 = do
      continueGame (payWinner (fst playersAndPot) (filter getPlayerIngame (fst playersAndPot),snd playersAndPot)) n
      return False 
  -- ein Spieler hat AllIn gesetzt -> Showdown 
  | any (<= 0) $ map getPlayerCash $ fst playersAndPot = do
      endgame playersAndPot (completeTableCards deck tisch) n
      return False
  -- Spiel geht normal weiter
  | otherwise = do
      return True 

-- Showdown + Entscheidung, ob noch ein weiteres Spiel gespielt wird oder nicht
endgame :: ([Player],Int) -> [Card] -> Int -> IO ()
endgame playersAndPot tischkarten n = do
  putStrLn "Showdown! Die Tischkarten sind: "
  putStrLn $ show tischkarten
  --Showdown, wer hat gewonnen
  playersAfterShowdown <- showdown playersAndPot tischkarten 
  -- noch eine Runde spielen?
  continueGame playersAfterShowdown n

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
  jeder (p,pot) tisch $ length p
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
            -- alle Spieler, die noch ingame sind, haben den gleichen Wettbetrag -> Setzrunde vorbei
            | all (== maximum (map getCurrentBet (p1:p2:ps))) (map getCurrentBet (playerIngame (p1:p2:ps))) = 
                                                return ((p1:p2:ps),pot)
            -- nur noch ein Spieler im Spiel -> beende
            | length (filter getPlayerIngame (p1:p2:ps)) <= 1 = return ((p1:p2:ps),pot) 
            -- | (getPlayerIngame p1) && (all (==False) $ map getPlayerIngame (p2:ps)) = return ((p1:p2:ps),pot)
            --sonst: naechster Spieler ist dran
            | otherwise = (rundeImmer ((p1:p2:ps),pot) tisch) >>= (\x -> wdhRunde x tisch)
          -- Jeder Spieler ist am Anfang der Runde mind.1 Mal dran. n ist length (p)
          jeder (p,pot) tisch 0 = wdhRunde (p,pot) tisch
          jeder (p,pot) tisch n = (rundeImmer (p,pot) tisch) >>= (\x -> jeder x tisch (n-1))
          --naechster Player kommt an Anfang der Liste (1.Player an den Schluss)  
          nextPlayer :: ([Player],Int) -> ([Player],Int)
          nextPlayer ((p1:ps),pot) = ((ps ++ [p1]),pot)
          --alle Spieler, die mitspielen
          playerIngame :: [Player] -> [Player]
          playerIngame ps = filter getPlayerIngame ps

-- Entscheidung: weiterspielen oder aufhoeren?
continueGame :: [Player] -> Int -> IO ()
continueGame ps n = do
    putStrLn ("Die " ++ show n ++ ". Spielrunde ist vorbei. Weiterspielen? (Y/N)")
    input <- getLine
    if (input=="n" || input=="N") 
      then do
        putStrLn "Du hast das Spiel beendet."
    else if (input=="y" || input=="Y") 
      then do
        putStrLn "Die naechste Spielrunde beginnt gleich!"
        let updatedPlayers = resetIngame $ resetCombos $ resetHands $ resetBets $ filter playerHasCash ps
        if (length updatedPlayers > 1) 
          then do
            startGame updatedPlayers $ n+1
          else do
            putStrLn "Es gibt nur noch einen Spieler! Es findet doch keine neue Runde statt."
    else do
        continueGame ps n

