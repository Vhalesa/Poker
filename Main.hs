module Main where

import Cards
import Chips

main :: IO()
main = do
    let c = Card (Spades, Five)
    let ch = Chip 50

    putStrLn ("Karte " ++ show c ++ " und Chip " ++ show ch)
