-- Die Menge der Chips

module Chips where

data Chip = Chip {value :: int}
	deriving (Eq, Ord)

instance Show Chip where
	show (Chip v) = show v
	