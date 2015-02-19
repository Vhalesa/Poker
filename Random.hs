module Random where
    
import System.Random
import Cards
import Data.List

-- Gibt es zufällige Zahl zwischen 0 und 51 zurück
-- Anzahl Karten des Pokerdecks = 52
shuffle :: Int -> IO Int
shuffle x = getStdRandom (randomR (0,x))

-- Gibt eine zufällige Karte des gesamten (geordneten) Decks zurück
getRandomCard = shuffle 52 >>= (\x -> return ((cards) !! x))

-- Gibt, falls moeglich, eine zufaellige Karte aus dem (evtl. vermindertem) Kartendeck, sowie das Kartendeck OHNE diese Karte aus
pullCard :: [Card] -> (Maybe Card,[Card])
pullCard [] = (Nothing,[])
pullCard cs = undefined --TODO

--