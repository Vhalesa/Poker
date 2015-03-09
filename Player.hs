{-# LANGUAGE NamedFieldPuns #-}
--Dieses Modul verwaltet Spieler quasi objektorientiert. 
module Player where
import Cards
import Combos
--import Chips

import Data.List

--Datentyp, welche Rolle der Spieler gerade annimmt (interessant bei 3+ Spielern)
data Role = BigBlind | SmallBlind | Dealer | None
    deriving (Eq)

--Datentyp fuer Spieler
data Player = Player {name::String, hand::[Card], combo::ScoreCombo, cash::Int, role::Role, currentBet::Int, ingame::Bool, ki::Bool}

instance Show Player where
    show Player {name,cash} = "(" ++ name ++ ", Cash: " ++ show cash ++")"

--Zwei Spieler mit dem selben Namen sind der selbe Spieler
instance Eq Player where
    a == b = getPlayerName a == getPlayerName b

getPlayerName :: Player -> String
getPlayerName Player {name} = name

setPlayerName :: String -> Player -> Player
setPlayerName name x = x {name}

getPlayerHand :: Player -> [Card]
getPlayerHand Player {hand} = hand

setPlayerHand :: [Card] -> Player -> Player
setPlayerHand hand x = x {hand}

getPlayerCombo :: Player -> ScoreCombo
getPlayerCombo Player {combo} = combo

-- Ermittelt fuer einen Spieler anhand der uebergebenen (Tisch-)Karten die Combo fuer den Spieler
-- und traegt diese im Spieler ein.
getComboForPlayer :: [Card] -> Player -> Player
getComboForPlayer cs p = setPlayerCombo (checkCombo (reverse $ sort $ getPlayerHand p ++ cs)) p

setPlayerCombo :: ScoreCombo -> Player -> Player
setPlayerCombo combo x = x {combo}

playerHasCash :: Player -> Bool
playerHasCash Player {cash} = cash > 0

getPlayerCash :: Player -> Int
getPlayerCash Player {cash} = cash

setPlayerCash :: Int -> Player -> Player
setPlayerCash cash x = x {cash}

getPlayerRole :: Player -> Role
getPlayerRole Player {role} = role

setPlayerRole :: Role -> Player -> Player
setPlayerRole role x = x {role}

getCurrentBet :: Player -> Int
getCurrentBet Player {currentBet} =currentBet

setCurrentBet ::  Int -> Player -> Player
setCurrentBet currentBet x = x {currentBet}

removeCurrentBet :: Player -> Player
removeCurrentBet p = setCurrentBet 0 p

getPlayerIngame :: Player -> Bool
getPlayerIngame Player {ingame} = ingame

setPlayerIngame :: Bool -> Player -> Player
setPlayerIngame ingame x = x {ingame}

getKI :: Player -> Bool
getKI Player {ki} = ki

setKI :: Bool -> Player -> Player
setKI ki x = x {ki}