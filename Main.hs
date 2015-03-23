--Dieses Modul fliegt vermutlich spaeter raus. Aber zum Testen ist/war es gut
module Main where

import System.Random

import Random
import Cards
--import Chips
import Combos
import KICalculation

--Diese Main Methode ist vorlaeufig zu Testzwecken bestimmt.
main :: IO()
main = do 
       test
  --     mischen

test = do
    let c1 = Card (Spades, Five)
        c2 = Card (Clubs, Five)
        ci = Card (Diamonds, Five)
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
    let mycards = [c9,c8,c7,c6,c5,c4,c3,c2,c1]
        onePair = [c1,c2,c3,c4,c5,c6]
        drilling = [c1,c2,ci,c4,c5]
        twoPair = [c1,c2,c7,c8]
        house = [c1,c2,ci,c7,c8]
    
    putStrLn $ show $ calculateVierlingChance $ take 2 onePair
    putStrLn $ show $ calculateVierlingChance $ take 5 drilling 
    putStrLn $ show $ calculateVierlingChance $ take 4 twoPair 
    putStrLn $ show $ calculateVierlingChance $ take 5 house 
    --putStrLn $ show $ checkCombo mycards
    --putStrLn $ show $ calculateFlushChance mycards


-- Gibt gemischtes Kartendeck aus (im Moment auf die Kommandozeile)
mischen = do
  randomNum <- randomIO :: IO Int
  let generator = makeGenerator randomNum
  let mixedDeck = shuffle cards generator []
  print mixedDeck


