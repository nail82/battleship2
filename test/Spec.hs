import Battle
import GameUtils

main :: IO ()
main = do
  let battleship = shipEmplacement Battleship East (0,0)
      ptship = shipEmplacement Pt South (1,5)
      carrier = shipEmplacement Carrier West (1,1)
  print $ show battleship
  print $ show ptship
  print $ show carrier
