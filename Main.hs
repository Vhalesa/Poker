module Main where

import Cards
import Chips
import Combos

--Diese Main Methode ist vorlaeufig zu Testzwecken bestimmt.
main :: IO()
main = test

test = do
    let c1 = Card (Spades, Five)
        c2 = Card (Spades, Six)
        c3 = Card (Diamonds, Seven)
        c4 = Card (Hearts, Eight)
        c5 = Card (Hearts, Nine)
    let ch = Chip 500
        co = Straight (c1,c2,c3,c4,c5)

    putStrLn ("Karte " ++ show c1 ++ " und Chip " ++ show ch)
    putStrLn ("Gesamtsumme der Chips jedes Spielers ist: " ++ show (Chips.sum chips))

    putStrLn ("Testspieler hat " ++ show co )