module Random where
import System.Random
import Cards

-- erzeugt einen Zufallsgenerator aus einem Int
makeGenerator :: Int -> StdGen
makeGenerator x = mkStdGen x

-- Erzeuge eine Zufallszahl zwischen 0 und x, sowie einen neuen Zufallsgenerator
-- Es muss ein RandomGenerator gen sowie die Obergrenze x uebergeben werden
randomizer :: StdGen -> Int -> (Int, StdGen)
randomizer gen x = randomR (0,x) gen

-- Gibt die n.te Karte des übergebenden Kartenstapels (beliebiger Groesse) zurueck
getCard :: [Card] -> Int -> Card
getCard stapel n = stapel !! n

-- Entfernt ein Element aus einer Liste und gibt die Liste ohne das Element zurueck
removeElem :: Int -> [a] -> [a]
removeElem n xs =
  let (ys,zs) = splitAt n xs
  in ys ++ (tail zs)

-- Mische das Deck
shuffle :: [Card] -> StdGen -> [Card] -> [Card]
shuffle [] gen newDeck = newDeck
shuffle oldDeck gen newDeck = shuffle (removeElem (randomX oldDeck gen) oldDeck) (newGenX oldDeck gen) 
  ((getCard oldDeck (randomX oldDeck gen)) : newDeck)

randomX :: [Card] -> StdGen -> Int
randomX oldDeck gen = fst $ randomizer gen $ (length oldDeck) -1

newGenX oldDeck gen = snd $ randomizer gen $ (length oldDeck) -1
