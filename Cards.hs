-- Definitione der Karten --
module Cards where

  data Color = Diamonds | Hearts | Spades | Clubs
    deriving (Enum, Bounded)

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

  -- Alle 4 Farben sind gleichwertig
  instance Eq Color where 
    a == b = True

  instance Ord Color where
    compare a b = EQ 

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

  --Liste mit allen Karten-Werten (Zwei bis Ass)
  values :: [Value]
  values = [minBound .. maxBound]

  -- Liste mit allen 4 Farben
  colors :: [Color]
  colors = [minBound .. maxBound]

  -- Liste mit allen Karten eines Decks (sortiert)
  cards :: [Card]
  cards = [Card (c,v) | c <- colors, v <- values]
