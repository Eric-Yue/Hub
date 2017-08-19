module Quadrants where
    
-- | Quadrants
--
-- >>> quadrants 1 3
-- 1
-- 
-- >>> quadrants (-1) 2
-- 2
--
-- >>> quadrants (-3) (-3)
-- 3
--
-- >>> quadrants 6 (-2)
-- 4

quadrants :: Integer -> Integer -> Integer
quadrants x y
  | x > 0 && y > 0 = 1
  | x < 0 && y > 0 = 2
  | x < 0 && y < 0 = 3
  | x > 0 && y < 0 = 4
  | otherwise = 0