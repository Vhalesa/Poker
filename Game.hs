module Game where
--bla spiellogik
import Cards
import Chips

import System.Random

-- Gibt eine zufaellige Karte aus dem Kartendeck, sowie das Kartendeck OHNE diese Karte aus
pullCard :: [Card] -> (Maybe Card,[Card])
pullCard [] = (Nothing,[])
pullCard cs = randomCard cs $ randomInt $ length cs
    where
        randomCard :: [Card] -> Int -> (Maybe Card,[Card])
        randomCard cs x = undefined

randomInt :: Int -> IO Int
randomInt x
    | x<=1 = getStdRandom (randomR (1,1)) -- immer 1
    | otherwise = getStdRandom (randomR (1,x)) -- Zufallszahl zwischen 1 und x


