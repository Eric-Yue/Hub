module CustomList where
    
-- | Custom List
--
-- >>> headList test5
-- 3
--
-- >>> tailList test5
-- Empty
--
-- >>> lastList test2
-- True
--
-- >>> headList $ tailList test1
-- 2
--
-- >>> lengthList test5
-- 1
--
-- >>> lengthList test4
-- 0
--
-- >>> prependList 2 test1
-- Cons 2 (Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 (Cons 6 (Cons 7 (Cons 8 (Cons 9 (Cons 10 Empty))))))))))
--
-- >>> appendList False test2
-- Cons True (Cons True (Cons False (Cons False (Cons False (Cons True (Cons False Empty))))))
--
-- >>> prependList 3.5 test4
-- Cons 3.5 Empty
--
-- >>> appendList 3.7 test4
-- Cons 3.7 Empty

data List a = Empty | Cons a (List a) 
    deriving (Show, Eq)

headList :: List a -> a
headList m = case m of
    Empty -> error "not exist"
    Cons x _ -> x

tailList :: List a -> List a
tailList m = case m of
    Empty -> error "not exist"
    Cons _ xs -> xs

lastList :: List a -> a
lastList m = case m of
    Empty -> error "not exist"
    Cons x Empty -> x
    Cons _ xs -> lastList xs

lengthList :: List a -> Integer
lengthList m = case m of
    Empty -> 0
    Cons _ Empty -> 1
    Cons _ xs -> lengthList xs + 1

prependList :: a -> List a -> List a
prependList a m = case m of
    Empty -> Cons a Empty
    Cons x xs -> Cons a (Cons x xs)

appendList :: a -> List a -> List a
appendList a m = case m of
    Empty -> Cons a Empty
    Cons x xs -> Cons x ( appendList a xs )


test1 :: List Integer
test1 = Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 (Cons 6 (Cons 7 (Cons 8 (Cons 9 (Cons 10 Empty)))))))))

test2 :: List Bool
test2 = Cons True (Cons True (Cons False (Cons False (Cons False (Cons True Empty)))))

test3 :: List Double
test3 = Cons 1.3 (Cons 5.2 (Cons 3.0 (Cons 8.7 (Cons 9.1 Empty))))

test4 :: List a
test4 = Empty

test5 :: List Int
test5 = Cons 3 Empty