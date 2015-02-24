{-# LANGUAGE NamedFieldPuns #-}
module Player where
import Cards
import Chips

--Datentyp, welche Rolle der Spieler gerade annimmt (interessant bei 3+ Spielern)
data Role = BigBlind | SmallBlind | Dealer | None

--Datentyp fuer Spieler
data Player = Player {name::String, hand::[Card], cash::[Chip], role::Role, currentBet::[Chip], ki::Bool}

getPlayerName :: Player -> String
getPlayerName Player {name} = name

setPlayerName :: Player -> String -> Player
setPlayerName x name = x {name}

getPlayerHand :: Player -> [Card]
getPlayerHand Player {hand} = hand

setPlayerHand :: Player -> [Card] -> Player
setPlayerHand x hand = x {hand}

getPlayerCash :: Player -> [Chip]
getPlayerCash Player {cash} = cash

setPlayerCash :: Player -> [Chip] -> Player
setPlayerCash x cash = x {cash}

getPlayerRole :: Player -> Role
getPlayerRole Player {role} = role

setPlayerRole :: Player -> Role -> Player
setPlayerRole x role = x {role}

getCurrentBet :: Player -> [Chip]
getCurrentBet Player {currentBet} =currentBet

setCurrentBet :: Player -> [Chip] -> Player
setCurrentBet x currentBet = x {currentBet}

getKI :: Player -> Bool
getKI Player {ki} = ki

setKI :: Player -> Bool -> Player
setKI x ki = x {ki}