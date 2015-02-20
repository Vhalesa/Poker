module Main where

import Cards
import Chips
import Combos

--Diese Main Methode ist vorlaeufig zu Testzwecken bestimmt.
main :: IO()
main = test

test = do
    let c1 = Card (Spades, Five)
        c2 = Card (Clubs, Five)
        c3 = Card (Diamonds, Seven)
        c4 = Card (Hearts, Eight)
        c5 = Card (Hearts, Nine)
        c6 = Card (Hearts, Ten)
        c7 = Card (Hearts, Jack)
        c8 = Card (Hearts, Queen)
        c9 = Card (Spades, King)
        co1 = Straight (c5,c6,c7,c8,c9)
        co2 = StraightFlush (c4,c5,c6,c7,c8)
   
    putStrLn ("Testspieler hat " ++ show co1 )

    putStrLn (show (c2 == c1))
    