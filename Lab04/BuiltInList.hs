module BuiltInList where

-- | Built In Lists
--
-- >>> head' [1 .. 10]
-- 1
--
-- >>> tail' [1 .. 100]
-- [2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100]
--
-- >>> length' [1 .. 200]
-- 200
--
-- >>> prepend' 2 [1 .. 3]
-- [2,1,2,3]
--
-- >>> append' [1, 2, 3, 4, 5] [4, 1, 3, 2]
-- [1,2,3,4,5,4,1,3,2]
--
-- >>> append' [1 .. 10] []
-- [1,2,3,4,5,6,7,8,9,10]
--
-- >>> swapFirstTwo [1, 3, 5, 7, 2]
-- [3,1,5,7,2]
--
-- >>> swapLastTwo [1, 3, 5, 2, 3]
-- [1,3,5,3,2]
--
-- >>> reverse' [1, 2, 3, 4, 5]
-- [5,4,3,2,1]
--
-- >>> reverse' []
-- []

head' :: [a] -> a
head' list = case list of
    []    -> error "No head for empty list"
    x : _ -> x

tail' :: [a] -> [a]
tail' list = case list of
    []     -> error "No tail for empty list"
    _ : xs -> xs

last' :: [a] -> a
last' list = case list of
    [] -> error "No last element for empty list"
    c : [] -> c
    _ : a -> last' a


length' :: [a] -> Integer
length' list = case list of
    [] -> 0
    _ : [] -> 1
    _ : g -> length' g + 1

prepend' :: a -> [a] -> [a]
prepend' a list = case list of
    [] -> a : []                        ---why this line is necessary? f : h cannot be [] ?
    f : h -> a : (f : h )

append' :: [a] -> [a] -> [a]
append' list1 list2 = list1 ++ list2

swapFirstTwo :: [a] -> [a]
swapFirstTwo list = case list of
    [] -> []                            ---why this line is necessary? a : ( b : ls )cannot be [] ?
    a : ( b : ls) -> b : ( a : ls)

swapLastTwo :: [a] -> [a]
swapLastTwo list = case list of
    [] -> []
    a : ( b : [] ) -> b : ( a : [] )
    a : ( b : c ) -> a : swapLastTwo ( b : c )

reverse' :: [a] -> [a]
reverse' list = case list of
    [] -> []
    a : b -> reverse' b ++ [ a ]
