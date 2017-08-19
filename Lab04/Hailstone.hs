module Hailstone where
    
-- | Hailstone
-- 
-- >>> hailstone 27
-- [27,82,41,124,62,31,94,47,142,71,214,107,322,161,484,242,121,364,182,91,274,137,412,206,103,310,155,466,233,700,350,175,526,263,790,395,1186,593,1780,890,445,1336,668,334,167,502,251,754,377,1132,566,283,850,425,1276,638,319,958,479,1438,719,2158,1079,3238,1619,4858,2429,7288,3644,1822,911,2734,1367,4102,2051,6154,3077,9232,4616,2308,1154,577,1732,866,433,1300,650,325,976,488,244,122,61,184,92,46,23,70,35,106,53,160,80,40,20,10,5,16,8,4,2,1]
--
-- >>> hailstone 3
-- [3,10,5,16,8,4,2,1]
--
-- >>> hailstone 13004
-- [13004,6502,3251,9754,4877,14632,7316,3658,1829,5488,2744,1372,686,343,1030,515,1546,773,2320,1160,580,290,145,436,218,109,328,164,82,41,124,62,31,94,47,142,71,214,107,322,161,484,242,121,364,182,91,274,137,412,206,103,310,155,466,233,700,350,175,526,263,790,395,1186,593,1780,890,445,1336,668,334,167,502,251,754,377,1132,566,283,850,425,1276,638,319,958,479,1438,719,2158,1079,3238,1619,4858,2429,7288,3644,1822,911,2734,1367,4102,2051,6154,3077,9232,4616,2308,1154,577,1732,866,433,1300,650,325,976,488,244,122,61,184,92,46,23,70,35,106,53,160,80,40,20,10,5,16,8,4,2,1]
--
-- >>> hailstone 129387
-- [129387,388162,194081,582244,291122,145561,436684,218342,109171,327514,163757,491272,245636,122818,61409,184228,92114,46057,138172,69086,34543,103630,51815,155446,77723,233170,116585,349756,174878,87439,262318,131159,393478,196739,590218,295109,885328,442664,221332,110666,55333,166000,83000,41500,20750,10375,31126,15563,46690,23345,70036,35018,17509,52528,26264,13132,6566,3283,9850,4925,14776,7388,3694,1847,5542,2771,8314,4157,12472,6236,3118,1559,4678,2339,7018,3509,10528,5264,2632,1316,658,329,988,494,247,742,371,1114,557,1672,836,418,209,628,314,157,472,236,118,59,178,89,268,134,67,202,101,304,152,76,38,19,58,29,88,44,22,11,34,17,52,26,13,40,20,10,5,16,8,4,2,1]

hailstone :: Integer -> [Integer]    ---(use case)
hailstone a = case a of
      1 -> 1 : []                 --- what if this is wrong,and I want to output the error "Hailstone is Wrong"
      otherwise -> case (a `mod` 2) of
                              0 -> a : hailstone ( div a 2 )
                              1 -> a : hailstone ( 3 * a + 1)

--- other solution:(use Guards)
---
--- hailstone :: Integer -> [Integer]
--- hailstone a
---     | a == 1 = 1 : []
---     | odd a = a : ( hailstone ( 3 * a + 1 ))
---     | even a = a : ( hailstone ( div a 2 ))
---     | otherwise = error "Hailstone is wrong"