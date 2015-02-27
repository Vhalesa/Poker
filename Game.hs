module Game where
--bla spiellogik
import Cards
--import Chips
import Random
import Combos
import Player

import System.Random
import Data.List

-- Erst Startfunktionen bei Spielstart, dann Endlos weitere Spielrunden, bis Spiel verlassen wird
main = do 
    let player1 = Player { name = "Player 1", hand = [], combo = HighCard [], cash = 4000, ki = False, role=BigBlind, currentBet=0}
        player2 = Player { name = "Player 2", hand = [], combo = HighCard [], cash = 4000, ki = True, role=SmallBlind, currentBet=0}
    startGame [player1,player2]

--alle Methoden, die fuer den Spielablauf benoetigt werden

--Ein komplettes Spiel mit einer Liste an Spielern durchfuehren
startGame ps = do
    --Karten mischen
    deck <- mischen
    print deck

    --Blinds bezahlen 
    playersAndPot <- doBlinds ps 10
    
    --Runde 1 ausfuehren: Karten austeilen und Spieler duerfen setzen,...
    --Karten austeilen 
    let
        r1 = runde1b deck $ fst playersAndPot
        players1 = fst $ r1
        deck1 = snd $ r1
    --ToDo: Setzen

    --Runde 2 ausfuehren: 3 Karten als Flop austeilen und wieder setzen
    r2 <- runde2 deck1
    let
        deck2 = last r2
    --ToDo

    --Runde 3 ausfuehren: 1 Karte als Turn austeilen und wieder setzen
    r3 <- runde3 deck2
    let
        deck3 = last r3
        tischkarten = head r2 ++ head r3
    --ToDo 

    --Runde 4 ausfuehren: 1 Karte als River austeilen und wieder setzen
    r4 <- runde4 deck3
    let
        finalTischkarten = tischkarten ++ head r4
    --ToDo

    --Showdown, wer hat gewonnen??
    playersAfterShowdown <- showdown (players1,snd $ playersAndPot) (finalTischkarten)

    --Weiterspielen?
    --ToDo

    -- Testzeug
    x <- entscheidungKI (ps,0) []
    print $ snd x
        
    

--Gibt gemischtes Kartendeck zurueck 
mischen = do
  randomNum <- randomIO :: IO Int 
  let generator = makeGenerator randomNum
      mixedDeck = shuffle cards generator []
  return mixedDeck

--Kuemmert sich um alles, was die Blinds angeht, Blinds zuweisen, bezahlen und in den Pot packen.
doBlinds :: [Player] -> Int -> IO([Player],Int)
doBlinds ps x = do
    let ps1 = map delegateBlind ps
        ps2 = map (payBlind x) ps1
        pot = blinds x
    putStrLn ("Der Pot betraegt nach den Blinds " ++ show pot)
    putStrLn ("Spieler 1 hat noch " ++ show (getPlayerCash $ head ps2) ++ " Chips")
    putStrLn ("Spieler 2 hat noch " ++ show (getPlayerCash $ last ps2) ++ " Chips")
    return (ps2,pot)
        

-- Small und Big Blind zuweisen
delegateBlind :: Player -> Player
delegateBlind p
    | getPlayerRole p == BigBlind = p {role = SmallBlind}
    | getPlayerRole p == SmallBlind = p {role = BigBlind}
    | otherwise = p

-- Blinds kommen in den Pot; Uebergabeparameter = Small Blind
blinds :: Int -> Int
blinds v = 3*v

-- Spieler muss Blind bezahlen Uebergabeparameter = Small Blind
payBlind :: Int -> Player -> Player
payBlind v p
    | getPlayerRole p == BigBlind = pay (2*v) p
    | getPlayerRole p == SmallBlind = pay v p
    | otherwise = p


-- p1 erhoeht 
-- bekommt die Liste der Player und den Pot (und gibt diese mit Veraenderung wieder zurueck)
call :: ([Player],Int) -> ([Player],Int)
call p = raise p 0

-- p1 erhoeht um betrag 
-- bekommt die Liste der Player, den Pot und den erhoehten Betrag 
raise :: ([Player],Int) -> Int -> ([Player],Int)
raise ((p1:p2:ps),pot) betrag = (p2:ps ++ [pay diff p1], pot + diff)
  where diff = getCurrentBet p2 - getCurrentBet p1 + betrag

-- Spieler bezahlt aus seinem Geld einen bestimmten Betrag
pay :: Int -> Player -> Player
pay 0 p = p
pay x p = p {cash = (getPlayerCash p)-x, currentBet = (getCurrentBet p) + x}
    
-- Der CurrentBet aller Spieler wird wieder auf 0 gesetzt (vor jeder Setzrunde erforderlich)
resetBets :: [Player] -> [Player]
resetBets ps = map removeCurrentBet ps

-- Runde ohne Kartenaufdecken, das wurde davor schon gemacht
-- setzen, erhoehen....
-- es muessen die [Player], der Pot und die Tischkarten uebergeben werden
--runde :: ([Player],Int) -> Int -> ([Player],Int)
--runde ((p1:p2:ps), pot) tisch = do
--  rundeImmer p1
--  rundeImmer p2
--  wdhRunde ((p1:p2:ps),pot)
--  print 1
--    where rundeImmer Player {ki=False} = entscheidungMensch 
--          rundeImmer Player {ki=True} = entscheidungKI
--          --lieber rekursiv
--          wdhRunde :: ([Player],Int) -> ([Player],Int)
--          wdhRunde p pot = undefined
--          --wdhRunde ((p1:p2:ps),pot) = do 
          --  when (betrag /=) $ do
          --    spielerDran rundeImmer
          --    naechsterSpiler dran
          --    wdhRunde

-- Abfrage beim Mensch: Call, Raise oder Fold?
-- braucht dazu die Player, den Pot und die Tischkarten
--entscheidungMensch :: ([Player],Int) -> Int -> ([Player],Int) 
-- mit IO
entscheidungMensch = undefined

-- Abfrage bei der KI: Call, Raise oder Fold?
-- braucht dazu die Player, den Pot und die Tischkarten
-- entscheidungKI :: ([Player],Int) -> Int -> ([Player],Int)
-- evlt mit IO, da ausgegeben werden soll, was die KI macht?
entscheidungKI :: ([Player],Int) -> [Card] -> IO ([Player],Int)
entscheidungKI (p, pot) tisch = do
    putStrLn "KI called"
    return $ call (p, pot)


-- Rund1,2,3,4 (jeweils das Karteaufdecken + Aufruf von runde)
-- mit Ausgabe, welche Karte gezogen wurde
--
-- 2 Karten werden fuer jeden Spieler gezogen
--runde1 :: [Card] -> [Player] -> IO
runde1 stapel p = do
  let cards1 = austeilen stapel 2 [] 2  
      p1 = setPlayerHand (head cards1) (head p) 
      p2 = setPlayerHand (cards1 !! 1) (p !! 1)
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

runde2 cs = do
    let
        --TrashCard ist die Oberste Karte, die laut Regeln vor jedem geben zur Seite gelegt wird
        trashCard = austeilen cs 1 [] 1
        cardsAndDeck = runde2b (last trashCard)
    putStrLn("Der Flop ist " ++ show (head cardsAndDeck))
    return cardsAndDeck

-- 1 Karte wird vom Stapel genommen (Funktioniert fuer Runde 3 und 4)
runde34 :: [Card] -> [[Card]]
runde34 cs = austeilen (last trashCard) 1 [] 1
    where 
        --TrashCard ist die Oberste Karte, die laut Regeln vor jedem geben zur Seite gelegt wird
        trashCard = austeilen cs 1 [] 1

runde3 cs = do
    let 
        cardsAndDeck = runde34 cs
    putStrLn("Die Turn Karte ist " ++ show (head cardsAndDeck))
    return cardsAndDeck


runde4 cs = do
    let 
        cardsAndDeck = runde34 cs
    putStrLn("Die River Karte ist " ++ show (head cardsAndDeck))
    return cardsAndDeck

--Testfunktion um 5 random Karten zu bekommen. Kann danach vermutlich wieder geloescht werden!!
runde1bis4 :: [Card] -> [Card]
runde1bis4 cs = f3 ++ s1 ++ t1 
    where
        f3 = head f
        s1 = head s
        t1 = head t
        f = runde2b cs
        s = runde34 $ last f
        t = runde34 $ last s

-- Zieht n mal jeweils x Karten, und gibt auch den Rest des Decks zurueck [[c1][c2]...[rest]]
-- Kann mit n = 1 genutzt werden, um Karten zum aufdecken zu ziehen
-- kann mit n >= 1 und x = 2 genutzt werden, um Spielern die Startkarten zu ziehen
austeilen :: [Card] -> Int -> [[Card]] -> Int -> [[Card]]
austeilen deck 0 erg x = erg ++ [deck]
austeilen deck n erg x = austeilen (snd $ splitAt x deck) (n-1) (erg ++ [(fst $ splitAt x deck)]) x
     
-- Showdown
--showdown :: ([Player],Int) -> IO
showdown (ps,pot) cs = do
    let
        playersWithCombo = map (getComboForPlayer cs) ps
        
        winner = playerWithHighestCombo playersWithCombo
        --Gewinner bezahlt negativen Betrag = Gewinner bekommt Betrag
        updatedWinner = map (pay (negate $ quot pot (length winner))) winner 

        newPlayerList = replace updatedWinner playersWithCombo
        
    putStrLn("Gewonnen hat " ++ show updatedWinner ++ " mit " ++ show (map getPlayerCombo winner) ++ "")
    putStrLn(show updatedWinner ++ " hat jetzt " ++ show (map getPlayerCash updatedWinner) ++ " Chips")

    return newPlayerList
    



-- Gibt den Spieler (bzw. die Spieler) mit der hoechsten Combo aus
-- Funktioniert fuer beliebig viele Spieler, bis auf die letzte Zeile
playerWithHighestCombo :: [Player] -> [Player]
playerWithHighestCombo [p1] = [p1]
playerWithHighestCombo (p1:p2:ps)
    | getPlayerCombo p1 > getPlayerCombo p2 = playerWithHighestCombo (p1:ps)
    | getPlayerCombo p1 < getPlayerCombo p2 = playerWithHighestCombo (p2:ps)
    | otherwise = [p1,p2] -- Das funktioniert aber nur bei 2 Spielern

replace :: [Player] -> [Player] -> [Player]
replace [] as = as
replace (n:ns) (a:as) = if (n==a) then n : replace ns as else a : replace (n:ns) as 


-- Entscheidung: weiterspielen oder aufhoeren?

-- Abfrage bei menschlichen Spieler fuer jede Wettrunde/Runde etc.
