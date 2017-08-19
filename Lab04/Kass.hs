module Kass where
    
-- | Kass
--
-- >>> beats Rock Rock
-- Draw
--
-- >>> beats Scissors Paper
-- Win
--
-- >>> beats Paper Lizard
-- Lose
--
-- >>> beats Rock Spock
-- Lose

data Move = Rock | Paper | Scissors | Lizard | Spock deriving (Eq)
data Result = Win | Lose | Draw deriving (Show)

beats :: Move -> Move -> Result
beats x y
   | x == Rock && y == Rock = Draw
   | x == Rock && y == Paper = Lose
   | x == Rock && y == Scissors = Win
   | x == Rock && y == Lizard = Win
   | x == Rock && y == Spock = Lose
   | x == Paper && y == Rock = Win
   | x == Paper && y == Paper = Draw
   | x == Paper && y == Scissors = Lose
   | x == Paper && y == Lizard = Lose
   | x == Paper && y == Spock = Win
   | x == Scissors && y == Rock = Lose
   | x == Scissors && y == Paper = Win
   | x == Scissors && y == Scissors = Draw
   | x == Scissors && y == Lizard = Win
   | x == Scissors && y == Spock = Lose
   | x == Lizard && y == Rock = Lose
   | x == Lizard && y == Paper = Win
   | x == Lizard && y == Scissors = Lose
   | x == Lizard && y == Lizard = Draw
   | x == Lizard && y == Spock = Win
   | x == Spock && y == Rock = Win
   | x == Spock && y == Paper = Lose
   | x == Spock && y == Scissors = Win
   | x == Spock && y == Lizard = Lose
   | x == Spock && y == Spock = Draw