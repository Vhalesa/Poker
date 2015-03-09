--Dieses Modul fliegt vermutlich spaeter raus. Aber zum Testen ist/war es gut
module Main where

import System.Random

import Random
import Cards
--import Chips
import Combos

--Diese Main Methode ist vorlaeufig zu Testzwecken bestimmt.
main :: IO()
main = do 
       test
  --     mischen

test = do
    let c1 = Card (Spades, Five)
        c2 = Card (Clubs, Five)
        c3 = Card (Diamonds, Seven)
        c4 = Card (Hearts, Eight)
        c5 = Card (Hearts, Nine)
        c6 = Card (Hearts, Ten)
        c7 = Card (Hearts, Jack)
        c8 = Card (Clubs, Jack)
        c9 = Card (Spades, King)
        co1 = Straight [c5,c6,c7,c8,c9]
        co2 = Straight [c4,c5,c6,c7,c8]
   
--    putStrLn ("Testspieler hat " ++ show co1 )

--    putStrLn (show (co2 == co1))

    putStrLn $ show $ checkCombo [c9,c8,c7,c6,c5,c4,c2,c1]


-- Gibt gemischtes Kartendeck aus (im Moment auf die Kommandozeile)
mischen = do
  randomNum <- randomIO :: IO Int
  let generator = makeGenerator randomNum
  let mixedDeck = shuffle cards generator []
  print mixedDeck


