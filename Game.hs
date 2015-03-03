#!/usr/bin/runhaskell
--Dieses Modul kuemmert sich um den Ablauf des Spiels. Hier werden Karten gegeben, Einsaetze gemacht usw.
module Game where


import Cards
--import Chips
import Random
import Combos
import Player

import System.Random
import Data.List
import Control.Monad

-- Erst Startfunktionen bei Spielstart, dann Endlos weitere Spielrunden, bis Spiel verlassen wird
main = do 
    let player1 = Player { name = "Player 1", hand = [], combo = HighCard [], cash = 4000, ki = False, role=BigBlind, ingame = True, currentBet=0}
        player2 = Player { name = "Awesome KI", hand = [], combo = HighCard [], cash = 4000, ki = True, role=SmallBlind, ingame = True, currentBet=0}
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
    let
        r1 = runde1b deck $ fst playersAndPot
        players1 = fst $ r1
        deck1 = snd $ r1

    -- Setzen -> Ueberpruefen, ob es schon einen Gewinner gibt?
    playersAndPot1 <- runde (players1, snd playersAndPot) []
    if ( not $ all getPlayerIngame $ tail (fst $ playersAndPot1))
      then do
        continueGame (payWinner (fst playersAndPot1) ([head $ fst playersAndPot1],snd playersAndPot1)) n
      else do 
        --Runde 2 ausfuehren: 3 Karten als Flop austeilen und wieder setzen
        r2 <- runde2 deck1
        let
            deck2 = last r2

        --Setzen
        playersAndPot2 <- runde playersAndPot1 (head r2)
        if ( not $ all getPlayerIngame $ tail (fst $ playersAndPot2))
          then do
            continueGame (payWinner (fst playersAndPot2) ([head $ fst playersAndPot2],snd playersAndPot2)) n
          else do
            --Runde 3 ausfuehren: 1 Karte als Turn austeilen und wieder setzen
            r3 <- runde3 deck2
            let
                deck3 = last r3
                tischkarten = head r2 ++ head r3

            --Setzen 
            playersAndPot3 <- runde playersAndPot2 tischkarten
            if ( not $ all getPlayerIngame $ tail (fst $ playersAndPot3))
              then do continueGame (payWinner (fst playersAndPot3) ([head $ fst playersAndPot3],snd playersAndPot3)) n
              else do
                --Runde 4 ausfuehren: 1 Karte als River austeilen und wieder setzen
                r4 <- runde4 deck3
                let
                    finalTischkarten = tischkarten ++ head r4

                --Setzen
                playersAndPot4 <- runde playersAndPot3 finalTischkarten
                if ( not $ all getPlayerIngame $ tail (fst $ playersAndPot4)) 
                  then do
                    continueGame (payWinner (fst playersAndPot4) ([head $ fst playersAndPot4],snd playersAndPot4)) n
                  else do
                    --Showdown, wer hat gewonnen??
                    playersAfterShowdown <- showdown (playersAndPot4) (finalTischkarten)

                    --Weiterspielen?
                    continueGame playersAfterShowdown n


--Gibt gemischtes Kartendeck zurueck 
mischen = do
  randomNum <- randomIO :: IO Int 
  let generator = makeGenerator randomNum
      mixedDeck = shuffle cards generator []
  return mixedDeck

--Kuemmert sich um alles, was die Blinds angeht, Blinds zuweisen, bezahlen und in den Pot packen.
doBlinds :: [Player] -> Int -> IO([Player],Int)
doBlinds ps x = do
    let ps1 = delegateBlind ps
        ps2 = map (payBlind x) ps1
        pot = blinds x
    putStrLn ("Der Pot betraegt nach den Blinds " ++ show pot)
    putStrLn ("Verbleibende Chips: " ++ show ps2)
    return (ps2,pot)
        

-- Small und Big Blind zuweisen
delegateBlind :: [Player] -> [Player]
delegateBlind [] = []
delegateBlind (p:ps)
    | getPlayerRole p == BigBlind = p {role = SmallBlind} : delegateBlind ps
    | getPlayerRole p == SmallBlind = delegateBlind ps ++ [p {role = BigBlind}]
    | otherwise = p : delegateBlind ps

-- Blinds kommen in den Pot; Uebergabeparameter = Small Blind
blinds :: Int -> Int
blinds v = 3*v

-- Spieler muss Blind bezahlen Uebergabeparameter = Small Blind
payBlind :: Int -> Player -> Player
payBlind v p
    | getPlayerRole p == BigBlind = pay (2*v) p
    | getPlayerRole p == SmallBlind = pay v p
    | otherwise = p

-- p1 folded
fold :: ([Player],Int) -> ([Player],Int)
fold ((p1:ps),pot) = (ps++[setPlayerIngame False p1],pot)

-- p1 erhoeht 
-- bekommt die Liste der Player und den Pot (und gibt diese mit Veraenderung wieder zurueck)
call :: ([Player],Int) -> ([Player],Int)
call p = raise p 0

-- p1 erhoeht um betrag 
-- bekommt die Liste der Player, den Pot und den erhoehten Betrag 
-- Reihenfolge der Spieler wird gleich um 1 verschoben -> naechster Spieler ist dran
raise :: ([Player],Int) -> Int -> ([Player],Int)
raise ((p1:p2:ps),pot) betrag = (p2:ps ++ [pay diff p1], pot + diff)
  where diff = (getCurrentBet p2 - getCurrentBet p1) + betrag

-- Spieler bezahlt aus seinem Geld einen bestimmten Betrag
pay :: Int -> Player -> Player
pay 0 p = p
pay x p = p {cash = (getPlayerCash p)-x, currentBet = (getCurrentBet p) + x}
    
-- Der CurrentBet aller Spieler wird wieder auf 0 gesetzt (vor jeder Setzrunde erforderlich)
resetBets :: [Player] -> [Player]
resetBets ps = map removeCurrentBet ps

resetHands :: [Player] -> [Player]
resetHands ps = map (setPlayerHand []) ps

resetCombos :: [Player] -> [Player]
resetCombos ps = map (setPlayerCombo $ HighCard []) ps

resetIngame :: [Player] -> [Player]
resetIngame ps = map (setPlayerIngame True) ps

-- Runde ohne Kartenaufdecken, das wurde davor schon gemacht
-- setzen, erhoehen....
-- es muessen die [Player], der Pot und die Tischkarten uebergeben werden
runde :: ([Player],Int) -> [Card]-> IO ([Player],Int)
runde (p, pot) tisch = do
  x <- rundeImmer (p,pot) tisch
  y <- rundeImmer x tisch
  wdhRunde y tisch
    where rundeImmer :: ([Player],Int) -> [Card] -> IO ([Player],Int) 
          rundeImmer ((p1:ps),pot) tisch = if (not $ getPlayerIngame p1)
                                             then return $ nextPlayer ((p1:ps),pot)
                                           else if (not $ all getPlayerIngame ps) 
                                              then return $ ((p1:ps),pot)
                                           else if (getKI p1) then entscheidungKI ((p1:ps),pot) tisch
                                           else entscheidungMensch ((p1:ps),pot) tisch
          wdhRunde :: ([Player],Int) -> [Card] -> IO ([Player],Int)
          wdhRunde ((p1:p2:ps),pot) tisch 
            | (getCurrentBet p1 == getCurrentBet p2) = return ((p1:p2:ps),pot)
            | (getPlayerIngame p1) && (not $ all getPlayerIngame (p2:ps)) = return ((p1:p2:ps),pot)
            | otherwise = (rundeImmer ((p1:p2:ps),pot) tisch) >>= (\x -> wdhRunde x tisch)
          nextPlayer :: ([Player],Int) -> ([Player],Int) --naechster Player kommt an Anfang der Liste (1. an den Schluss)  
          nextPlayer ((p1:ps),pot) = ((ps ++ [p1]),pot)

-- Abfrage beim Mensch: Call, Raise oder Fold?
-- braucht dazu die Player, den Pot und die Tischkarten
--entscheidungMensch :: ([Player],Int) -> Int -> IO ([Player],Int) 
-- mit IO
entscheidungMensch :: ([Player],Int) -> [Card] -> IO ([Player],Int)
entscheidungMensch (p, pot) tisch = do
    putStrLn ""
    putStrLn "Du bist dran"
    abfrage
    where abfrage = do  
            --Ausgabe: Du hast folgende Handkarten: ...
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
            --du hast noch xxx Geld (weiter Info Ausgaben)?
            let eigCash = getPlayerCash $ head p
                bet = getCurrentBet $ p !! 1
                eigBet = getCurrentBet $ head p
            putStr "Du hast noch Cash: "
            putStrLn . show $ eigCash 
            putStr "Die höchste Wette ist derzeit bei: "
            putStrLn . show $ bet 
            putStr "Deine Wette ist derzeit bei: " -- eher zu Debug-Zwecken (kann nachher evtl weg)
            putStrLn . show $ eigBet 
            putStrLn "Was möchtest du tun? (Call/Raise/Fold)"
            input <- getLine
            if (input == "Call" || input == "call") 
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
                raiseAbfrage eigCash eigBet bet 
            else do
              putStrLn "Du musst Call, Raise oder Fold eingeben."
              abfrage 
          --Betrag, um den der Spieler erhoehen will
          raiseAbfrage eigCash eigBet bet = do
            eingabe <- getLine
            let allIn = eigCash - (bet - eigBet) 
            if (null (isInt eingabe)) || ( (snd $ head $ isInt eingabe) /= "")
              then do  
                putStr "Du musst eine ganze Zahl eingeben, zwischen 0 und AllIn " 
                putStrLn . show $ allIn
                raiseAbfrage eigCash eigBet bet 
              else do
                let betrag = fst $ head (isInt eingabe)
                if (betrag < 0)
                  then do
                    putStrLn "Du kannst nicht um einen negativen Betrag erhöhen. Du Cheater!"
                    putStrLn "Gib gefälligst eine positive Zahl ein."
                    raiseAbfrage eigCash eigBet bet 
                else if (betrag > allIn)
                  then do
                    putStrLn "Du kannst nicht um mehr erhöhen als du Geld hast!"
                    putStr "Du musst eine ganze Zahl eingeben, zwischen 0 und AllIn " 
                    putStrLn . show $ allIn
                    raiseAbfrage eigCash eigBet bet 
                else do
                  putStrLn ("Du hast um den Betrag " ++ eingabe ++ " erhöht.")
                  return $ raise (p,pot) betrag 
          --gibt wenn Anfang ein Int einen [(Int,RestString)] zurueck. Ansonsten eine leere Liste
          isInt :: String -> [(Int,String)]
          isInt x = reads x
         
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

-- Rund1,2,3,4 (jeweils das Karteaufdecken + Aufruf von runde)
-- mit Ausgabe, welche Karte gezogen wurde
--
-- 2 Karten werden fuer jeden Spieler gezogen
--runde1 :: [Card] -> [Player] -> IO
runde1 stapel p = do
  let cards1 = austeilen stapel 2 [] 2  
      p1 = setPlayerHand (head cards1) (head p) 
      p2 = setPlayerHand (cards1 !! 1) (p !! 1)
  putStrLn ("Jeder Spieler hat seine Karten auf die Hand bekommen")
  return ([p1,p2], last cards1)

--runde1 nicht in IO. 
runde1b :: [Card] -> [Player] -> ([Player],[Card])
runde1b cs ps = ([p1,p2],last cards1)
                where cards1 = austeilen cs 2 [] 2  
                      p1 = setPlayerHand (head cards1) (head ps)
                      p2 = setPlayerHand (cards1 !! 1) (ps !! 1)

-- 3 Karten werden vom Stapel genommen
runde2b :: [Card] -> [[Card]]
runde2b cs = austeilen cs 1 [] 3

-- 1 Karte wird vom Stapel genommen (Funktioniert fuer Runde 3 und 4)
runde34 :: [Card] -> [[Card]]
runde34 cs = austeilen (last trashCard) 1 [] 1
    where 
        --TrashCard ist die Oberste Karte, die laut Regeln vor jedem geben zur Seite gelegt wird
        trashCard = austeilen cs 1 [] 1

--Gibt die ersten 3 Karten, den sog. Flop zurueck, sowie das restliche Deck
runde2 cs = do
    let
        --TrashCard ist die Oberste Karte, die laut Regeln vor jedem geben zur Seite gelegt wird
        trashCard = austeilen cs 1 [] 1
        cardsAndDeck = runde2b (last trashCard)
    putStrLn("Der Flop ist " ++ show (head cardsAndDeck))
    return cardsAndDeck

--Gibt die vierte Karte, den sog. Turn zurueck, sowie das restliche Deck
runde3 cs = do
    let 
        cardsAndDeck = runde34 cs
    putStrLn("Die Turn Karte ist " ++ show (head cardsAndDeck))
    return cardsAndDeck

--Gibt die fuenfte und letzte Karte, den sog. River, zurueck, OHNE das restliche Deck (Das brauchen wir nicht mehr)
runde4 cs = do
    let 
        cardsAndDeck = runde34 cs
    putStrLn("Die River Karte ist " ++ show (head cardsAndDeck))
    return cardsAndDeck

-- Zieht n mal jeweils x Karten, und gibt auch den Rest des Decks zurueck [[c1][c2]...[rest]]
-- Kann mit n = 1 genutzt werden, um Karten zum aufdecken zu ziehen
-- kann mit n >= 1 und x = 2 genutzt werden, um Spielern die Startkarten zu ziehen
-- Sollte aufgerufen werden: austeilen deck n [] x;
--  wenn erg /= [] werden die Karten in erg am Ende wieder mit ausgegeben
austeilen :: [Card] -> Int -> [[Card]] -> Int -> [[Card]]
austeilen deck 0 erg x = erg ++ [deck]
austeilen deck n erg x = austeilen (snd $ splitAt x deck) (n-1) (erg ++ [(fst $ splitAt x deck)]) x
     
-- Showdown
showdown :: ([Player],Int) -> [Card] -> IO [Player]
showdown (ps,pot) cs = do
    let
        playersWithCombo = map (getComboForPlayer cs) ps
        
        winner = playerWithHighestCombo playersWithCombo

        updatedPlayers = payWinner playersWithCombo (winner,pot) 

        
    putStrLn("Gewonnen hat " ++ (show (map getPlayerName winner)) ++ " mit " ++ show (map getPlayerCombo winner) ++ "")
    putStrLn( show updatedPlayers)

    return updatedPlayers

-- Gewinner bezahlt negativen Betrag = Gewinner bekommt Betrag  
payWinner :: [Player] -> ([Player],Int) -> [Player]
payWinner all (winners,pot) = replace newWinners all
    where
        newWinners = map (pay (negate $ quot pot (length winners))) winners


-- Gibt den Spieler (bzw. die Spieler) mit der hoechsten Combo aus
-- Funktioniert fuer beliebig viele Spieler, bis auf die letzte Zeile
playerWithHighestCombo :: [Player] -> [Player]
playerWithHighestCombo [p1] = [p1]
playerWithHighestCombo (p1:p2:ps)
    | getPlayerCombo p1 > getPlayerCombo p2 = playerWithHighestCombo (p1:ps)
    | getPlayerCombo p1 < getPlayerCombo p2 = playerWithHighestCombo (p2:ps)
    | otherwise = [p1,p2] -- Das funktioniert aber nur bei 2 Spielern

-- Ersetzt in der zweiten Liste die Spieler mit gleichem Namen wie in der ersten Liste.
-- Funktioniert im Moment nur, wenn beide Listen gleich geordnet sind
replace :: [Player] -> [Player] -> [Player]
replace [] as = as
replace (n:ns) (a:as) = if (n==a) then n : replace ns as else a : replace (n:ns) as 


-- Entscheidung: weiterspielen oder aufhoeren?

continueGame ps n = do
    putStrLn ("Die " ++ show n ++ ". Spielrunde ist vorbei. Weiterspielen? (Y/N)")
    input <- getLine
    if any (<=10) (map getPlayerCash ps) then do
        putStrLn "Mindestens ein Spieler hat zu wenig Geld. Das Spiel ist deshalb beendet."
        --todo falls wir mehr als 2 Spieler machen: Ohne den Spieler weiterspielen?
    else if input=="y" || input=="Y" then do
        putStrLn "Die naechste Spielrunde beginnt gleich!"
        let
            updatedPlayers = resetIngame $ resetCombos $ resetHands $ resetBets ps
        startGame updatedPlayers (n+1)
    else if input=="n" || input=="N" then do
        putStrLn "Du hast das Spiel beendet."
    else do
        continueGame ps n

