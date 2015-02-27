import Control.Monad 

main = do
  abfrage

abfrage = do 
  putStrLn "Was möchtest du tun? Call, Raise oder Fold?"
  input <- getLine
  if (input == "Call" || input == "call") 
    then do
      putStrLn "Du hast Call eingesetzt. It's very effektive" 
  else if (input  == "Fold" || input == "fold")
     then do 
      putStrLn "Du hast Fold eingesetzt. It's not very effektive" 
  else if (input  == "Raise" || input == "raise")
     then do 
      putStrLn "Du hast Raise eingesetzt. Um wie viel möchtest du erhöhen?" 
      betrag <- getLine
      print betrag
  else do
    putStrLn "Du musst entweder Raise oder Call oder Fold eingeben!"
    main

--gibt wenn Anfang ein Int einen [(Int,RestStrin)] zurueck. Ansonsten eine leere Liste
isInt :: String -> [(Int,String)]
isInt x = reads x

