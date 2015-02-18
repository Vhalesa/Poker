-- Definitione der Karten --
module Cards where

  data Color = Diamonds | Hearts | Spades | Clubs
    deriving (Ord, Eq)

  data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine
               | Ten | Jack | Queen | King | Ace
    deriving (Ord, Eq, Enum, Bounded)

  data Card = Card (Color, Value)
    deriving (Ord, Show, Eq)

  instance Show Color where
    show Diamonds = "♦"
    show Hearts = "♥"
    show Spades = "♠"
    show Clubs = "♣"

  instance Show Value where
    show Two = "2"
    show Three = "3"
    show Four = "4"
    show Five = "5"
    show Six = "6"
    show Seven = "7"
    show Eight = "8"
    show Nine = "9"
    show Ten = "10"
    show Jack = "Jack"
    show Queen = "Queen"
    show King = "King"
    show Ace = "Ace"

  values :: [Value]
  values = [minBound .. maxBound]
  

    

