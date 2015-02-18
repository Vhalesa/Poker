module Chips where

data Chip = Chip {value :: int}
	deriving (Show, Eq, Order)

instance Show Chip where
	show (Chip v) = show v