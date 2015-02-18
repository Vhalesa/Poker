module Chips where

data Chip = Chip {value :: Int}
        deriving (Eq, Ord)

instance Show Chip where
        show (Chip v) = show v
