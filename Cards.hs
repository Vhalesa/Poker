-- Definitione der Karten --
module Cards where

  data Color = Diamonds | Hearts | Spades | Clubs
    deriving (Order, Show, Eq)

  data Value = Ace | King | Queen | Jack | Ten | Nine | Eight | Seven | Six | Five 
                | Four | Three | Two 
    deriving (Order, Show, Eq)

  data Card = Card (Color, Value)
    deriving (Order, Show, Eq)

  instance Show Color where
    show Diamonds = "♦"
    show Hearts = "♥"
    show Spades = "♠"
    show Clubs = "♣"

  instance Show Value where
    show Ace = "Ace"
    show King = "King"
    show Queen = "Queen"
    show Jack = "Jack"
    show Ten = "10"
    show Nine = "9"
    show Eight = "8"
    show Seven = "7"
    show Six = "6"
    show Five = "5"
    show Four = "4"
    show Three = "3"
    show Two = "2"
