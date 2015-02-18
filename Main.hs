module Main where

import Cards
import Chips

main :: IO()
main = do
    let c = Card (Spades, Five)
    let ch = Chip 500

    putStrLn ("Karte " ++ show c ++ " und Chip " ++ show ch)
    putStrLn ("Gesamtsumme der Chips jedes Spielers ist: " ++ show (Chips.sum chips))