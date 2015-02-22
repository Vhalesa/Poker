-- Definitionen der Karten --
module Cards where

  data Color = Diamonds | Hearts | Spades | Clubs
    deriving (Eq, Ord, Enum, Bounded)

  data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine
               | Ten | Jack | Queen | King | Ace
    deriving (Ord, Eq, Enum, Bounded)

  data Card = Card (Color, Value)
    deriving (Show, Bounded)

  instance Show Color where
    show Diamonds = "♦"
    show Hearts = "♥"
    show Spades = "♠"
    show Clubs = "♣"

  instance Eq Card where
    (Card (c1,v1)) == (Card (c2,v2)) = v1 == v2

  instance Ord Card where
    compare (Card (c1,v1)) (Card (c2,v2)) = compare v1 v2

  -- Quelle: http://stackoverflow.com/questions/5684049/is-there-some-way-to-define-an-enum-in-haskell-that-wraps-around
  -- | a `succ` that wraps 
  -- wird im Moment nicht benoetigt, aber vielleicht spaeter?
  --succB :: (Bounded a, Enum a, Eq a) => a -> a 
  --succB en | en == maxBound = minBound
  --         | otherwise = succ en
  --
  -- | a `pred` that wraps
  -- damit Ace auch der Vorgaenger von Two ist
  predB :: (Bounded a, Enum a, Eq a) => a -> a
  predB en | en == minBound = maxBound
           | otherwise = pred en  

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
    show Jack = "J"
    show Queen = "Q"
    show King = "K"
    show Ace = "A"

  --Liste mit allen Karten-Werten (Zwei bis Ass)
  values :: [Value]
  values = [minBound .. maxBound]

  -- Liste mit allen 4 Farben
  colors :: [Color]
  colors = [minBound .. maxBound]

  -- Liste mit allen Karten eines Decks (sortiert)
  cards :: [Card]
  cards = [Card (c,v) | c <- colors, v <- values]

  getColor :: Card -> Color
  getColor (Card (c,v)) = c

  getValue :: Card -> Value
  getValue (Card (c,v)) = v


