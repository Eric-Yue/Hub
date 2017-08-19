module RPS where
    
-- | Rock Paper Scissors
--
-- >>> beats Rock
-- Paper
--
-- >>> beats Paper
-- Scissors
-- 
-- >>> beats Scissors
-- Rock

data Move = Rock | Paper | Scissors deriving (Show,Eq)

beats :: Move -> Move
beats x = case  x of
   Rock -> Paper
   Paper -> Scissors
   Scissors -> Rock
