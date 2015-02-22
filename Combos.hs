module Combos where

import Cards

import Data.List

--Die 5 Karten, die fuer jeden Spieler gewertet werden
data Combo = Combo (Card,Card,Card,Card,Card)

--Die Verschiedenen Typen von Wertungen
data ScoreCombo = HighCard [Card]
                | Pair [Card] 
                | Pair2 [Card]
                | ThreeC [Card]
                | Straight [Card]
                | Flush [Card]
                | FullHouse [Card]
                | FourC [Card]
                | StraightFlush [Card]
                | RoyalFlush [Card]
    deriving (Show, Ord, Eq)

--Ueberpruefe, welche Combo ein Spieler mit seiner Hand und den Tischkarten hat
-- Es muss eine Liste, die aus den Handkarten und den Tischkarten besteht uebergeben werden
-- (am besten absteigend geordet, oder die Funktion ordnet die Liste anfangs selbst)
checkCombo :: [Card] -> ScoreCombo
checkCombo cs
    -- Achtung: Straight Flush oder Royal Flush -> hier der Funktion checkStraight nur die Liste mit Karten gleicher Farbe
    -- uebergeben oder so 
    | checkFlush cs = if (checkStraight $ getFlush cs $ ind5 $ colorsIn cs []) 
                        then StraightFlush $ head $ getStraight $ getFlush cs $ ind5 $ colorsIn cs []
                        else Flush $ first5 $ getFlush cs $ ind5 $ colorsIn cs []
    | checkStraight cs = Straight $ head $ getStraight cs
    | checkFour cs = getFour cs --Vierling erstellen
    | fst $ checkThree cs = if (checkDifferentTwo cs (snd $ checkThree cs)) then getFullHouse cs --Full House erstellen
                            else getThree cs --Drilling erstellen
    | fst $ checkTwo cs = if (checkDifferentTwo cs (snd $ checkTwo cs)) then getPair2 cs else getPair cs--Zweites Pair ueberpruefen
    | otherwise = HighCard $ first5 cs --High Card erstellen
    where
        checkFlush :: [Card] -> (Bool)
        checkFlush cs = if (length cs <5) then False else any (>=5) (colorsIn cs [])

        checkStraight :: [Card] -> Bool
        checkStraight cs = if (length cs <5) then False else findStraight cs

        checkFour :: [Card] -> Bool
        --Da Liste geordnet, muss nur der erste Wert mit den 3 Nachfolgern verglichen werden
        checkFour (c1:c2:c3:c4:cs) = if (c1==c2 && c1==c3 && c1==c4) then True else checkFour (c2:c3:c4:cs)
        checkFour _ = False

        checkThree :: [Card] -> (Bool,Card)
        --Da Liste geordnet, muss nur der erste Wert mit den 2 Nachfolgern verglichen werden
        checkThree (c1:c2:c3:cs) = if (c1==c2 && c1==c3) then (True,c1) else checkThree (c2:c3:cs)
        checkThree _ = (False,Card (Spades,Ace))

        checkTwo :: [Card] -> (Bool,Card)
        --Da Liste geordnet, muss nur der erste Wert mit dem Nachfolger verglichen werden
        checkTwo (c1:c2:cs) = if (c1==c2) then (True,c1) else checkTwo (c2:cs)
        checkTwo _ = (False,Card (Spades,Ace))

        --Ueberprueft, ob noch ein zweites Paar, das unterschiedlich zur uebergebenen Karte ist, existiert
        checkDifferentTwo :: [Card] -> Card -> Bool
        --Da Liste geordnet, muss nur der erste Wert mit dem Nachfolger verglichen werden
        checkDifferentTwo (c1:c2:cs) cx= if (c1==c2 && c1/=cx) then True else checkDifferentTwo (c2:cs) cx
        checkDifferentTwo _ _= False

        --zaehlt an welcher Stelle einer Int Liste ein Wert >= 5 ist und gibt diesen Index zurueck
        ind5 :: [Int] -> Int
        ind5 [] = 1
        ind5 (h:is) = if h>=5 then 0 else 1+ind5 is

        first5 :: [a] -> [a]
        first5 (a1:a2:a3:a4:a5:as) = (a1:a2:a3:a4:a5:[])
        first5 _ = []
        
        --sucht eine Strasse -> erst Duplikate eliminieren
        --gibt an, ob es eine Strasse gibt, oder nicht
        findStraight :: [Card] -> Bool
        findStraight cs = not $ null $ getStraight (remDup cs [])

        -- Gibt den eine Liste mit Straights aus (bei 5-7 Karten)
        getStraight :: [Card] -> [[Card]]
        getStraight cs = map fst $ filter snd $ zip (splitListS cs) $ map straight $ splitListS cs
          where straight :: [Card] -> Bool -- Gibt fuer 5-elementige Liste aus, ob ein Straight drin ist
                straight cs = all (==True) $ splitList4 $ checkPreds cs []  
                     
                --teilt die Liste der Hand+Tischkarten in 4 Teillisten auf, wo jeweils ein Straight sein koennte
                splitListS :: [Card] -> [[Card]]
                splitListS cs = [fst $ splitAt 5 cs, tail $ fst $ splitAt 6 cs, snd $ splitAt 2 cs, last4 cs ++ [head cs]] 
        
                --Nimmt die ersten 4 Elemente aus einer Liste
                splitList4 :: [a] -> [a] 
                splitList4 cs = fst $ splitAt 4 cs
                
                --Nimmt die letzten 4 Elemente aus einer Liste
                last4 :: [a] -> [a]
                last4 cs = reverse $ splitList4 $ reverse cs
       
                --Checkt, welche Karte einen Vorgaenger hat
                checkPreds :: [Card] -> [Bool] -> [Bool]
                checkPreds [] erg = reverse erg 
                checkPreds cs erg = hilf cs cs erg 
                  where hilf cs [] erg = checkPreds [] erg --Hilfsfunktion, damit die urspruengliche Liste verwendet werden kann
                        hilf cs (c1:c) erg = hilf cs c $ (predInList c1 cs):erg

                        --prüft ob die VorgaengerKarte in der uebergeben Liste ist
                        predInList :: Card -> [Card] -> Bool
                        predInList c1 cs = elem (getPred c1) $ map getValue cs

                        -- gibt den Value der Vorgänger-Karte (naechstniedrigeren Karte) zurueck
                        getPred :: Card -> Value 
                        getPred (Card(c,v)) = predB v

        --getFlush erzeugt einen Flush aus einer Liste von Karten. Darf nur aufgerufen werden, wenn es auch einen Flush gibt
        --(sonst wird ein leerer CFlush erzeugt)
        getFlush :: [Card] -> Int -> [Card]
        getFlush cs i
                    | i==0 = getDFlush cs
                    | i==1 = getHFlush cs
                    | i==2 = getSFlush cs
                    | otherwise = getCFlush cs
        
        getDFlush [] = []
        getDFlush (ca:cs) = if getColor ca == Diamonds then ca : getDFlush cs else getDFlush cs
        getHFlush [] = []
        getHFlush (ca:cs) = if getColor ca == Hearts then ca : getHFlush cs else getHFlush cs
        getSFlush [] = []
        getSFlush (ca:cs) = if getColor ca == Spades then ca : getSFlush cs else getSFlush cs
        getCFlush [] = []
        getCFlush (ca:cs) = if getColor ca == Clubs then ca : getCFlush cs else getCFlush cs

        --getFour erzeugt einen Vierling mit verbleibender High Card
        getFour :: [Card] -> ScoreCombo
        getFour cs = FourC $ fours ++ high
            where
                fours = get4 cs
                high = getHighestSingle 1 cs fours

        --getFullHouse erzeugt ein Full House aus einer Liste von Karten
        getFullHouse :: [Card] -> ScoreCombo
        getFullHouse cs = FullHouse $ threes ++ twoes
            where
                threes = get3 cs
                twoes = get2Different cs $ head threes

        --getThree erzeugt einen Drilling mit den 2 verbleibenden HighCards
        getThree :: [Card] -> ScoreCombo
        getThree cs = ThreeC $ threes ++ twoHigh
            where
                threes = get3 cs
                twoHigh = getHighestSingle 2 cs threes

        getPair :: [Card] -> ScoreCombo
        getPair cs = Pair $ twoes ++ threeHigh
            where
                twoes = get2 cs
                threeHigh = getHighestSingle 3 cs twoes

        getPair2 :: [Card] -> ScoreCombo
        getPair2 cs = Pair2 $ twoes ++ twoes2 ++ high
            where
                twoes = get2 cs
                twoes2 = get2Different cs $ head twoes
                high = getHighestSingle 1 cs (twoes++twoes2)

        --Gibt einen Vierling als Liste von Karten
        get4 :: [Card] -> [Card]
        get4 (c1:c2:c3:c4:cs) = if c1==c2 && c1==c3 && c1==c4 then [c1,c2,c3,c4] else get3 (c2:c3:c4:cs)
        get4 _ = []

        --Gibt einen Drilling als Liste von Karten
        get3 :: [Card] -> [Card]
        get3 (c1:c2:c3:cs) = if c1==c2 && c1==c3 then [c1,c2,c3] else get3 (c2:c3:cs)
        get3 _ = []

        --Gibt ein Kartenpaar als Liste von Karten
        get2 :: [Card] -> [Card]
        get2 (c1:c2:cs) = if c1==c2 then [c1,c2] else get2 (c2:cs)
        get2 _ = []

        --Gibt ein Kartenpaar als Liste, das nicht gleich ist wie die uebergebene Karte cx
        get2Different :: [Card] -> Card -> [Card]
        get2Different (c1:c2:cs) cx = if (c1==c2 && c1/=cx) then [c1,c2] else get2Different (c2:cs) cx
        get2Different _ _= []

        --Gibt die hoechsten n Karten, die nicht in ks vorkommen, aber in cs
        getHighestSingle :: Int -> [Card] -> [Card] -> [Card]
        getHighestSingle 0 _ _ = []
        getHighestSingle n [] _ = []
        getHighestSingle n (c:cs) ks = if (any (==c) ks) then getHighestSingle n cs ks else c : getHighestSingle (n-1) cs ks
      
-- Diese Funktion zaehlt wie oft welche Farbe in einer Liste an Karten vorkommt.
-- Format: [Diamond , Hearts , Spades , Clubs]
colorsIn :: [Card] -> [Int] -> [Int]
colorsIn [] is = is
colorsIn (ca : cs) (d:h:s:c:is) 
            | getColor ca == Diamonds = colorsIn cs (d+1:h:s:c:[])
            | getColor ca == Hearts = colorsIn cs (d:h+1:s:c:[])
            | getColor ca == Spades = colorsIn cs (d:h:s+1:c:[])
            | otherwise = colorsIn cs (d:h:s:c+1:[])
colorsIn cs []= colorsIn cs [0,0,0,0]


-- Karten mit gleichem Value eliminieren (von jedem Value nur 1 Karte danach) (Farbe wird nicht beachtet)
-- Starten: remDup listeVonKarten []
remDup :: [Card] -> [Card] -> [Card]
remDup [] erg = reverse erg
remDup (c1:cs) erg = if (elem (getValue c1) $ map getValue cs) then remDup cs erg
                        else remDup cs $ c1:erg

