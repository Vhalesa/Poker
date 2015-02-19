import System.Random
import Cards
import Data.List

-- Gibt es zufällige Zahl zwischen 0 und 51 zurück
-- Anzahl Karten des Pokerdecks = 52
shuffle :: IO Int
shuffle = getStdRandom (randomR (0,51))

-- Gibt eine zufällige Karte des (geordneten) Decks zurück
getRandomCard = shuffle >>= (\x -> return ((cards) !! x))
