module Problems11 where

import Data.List

-- problem 11
-- λ> encodeModified "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']
data Enc a = Single a | Multiple Int a deriving (Show, Eq)

encode11 :: Eq a => [a] -> [Enc a]
encode11 [] = []
encode11 xs = map mapping . group $ xs
  where
    mapping [x] = Single x
    mapping ys = Multiple (length ys) (head ys)

-- problem 12
-- λ> decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']
-- "aaaabccaadeeee"
decodeModified :: Eq a => [Enc a] -> [a]
decodeModified = concatMap decode
  where
    decode (Single x) = [x]
    decode (Multiple c x) = replicate c x

-- problem 13
-- encodeDirect "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']
encode13 :: Eq a => [a] -> [Enc a]
encode13 [] = []
encode13 (x : xs) = go 1 x xs
  where
    go 1 v [] = [Single v]
    go c v [] = [Multiple c v]
    go c v (y : ys)
      | v == y = go (c + 1) v ys
      | c == 1 = Single v : encode13 (y : ys)
      | otherwise = Multiple c v : encode13 (y : ys)

-- problem 14
-- λ> dupli [1, 2, 3]
-- [1,1,2,2,3,3]
dupli :: [a] -> [a]
dupli [] = []
dupli (x : xs) = x : x : dupli xs

-- problem 15
-- λ> repli "abc" 3
-- "aaabbbccc"
repli :: [a] -> Int -> [a]
repli xs c = concatMap (replicate c) xs

-- Problem 16
-- λ> dropEvery "abcdefghik" 3
-- "abdeghk"
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs c = take (c - 1) xs ++ dropEvery (drop c xs) c

dropEvery' :: [a] -> Int -> [a]
dropEvery' [] _ = []
dropEvery' xs de = go xs 1
  where
    go [] _ = []
    go (y : ys) c
      | de == c = go ys 1
      | otherwise = y : go ys (c + 1)

-- problem 17
-- split "abcdefghik" 3
-- ("abc", "defghik")
split :: [a] -> Int -> ([a], [a])
split [] _ = ([], [])
split xs 0 = ([], xs)
split (x : xs) c = (x : fst next, snd next)
  where
    next = split xs (c - 1)

split' :: [a] -> Int -> ([a], [a])
split' = go []
  where
    go acc [] _ = (reverse acc, [])
    go acc ys 0 = (reverse acc, ys)
    go acc (y : ys) n = go (y : acc) ys (n - 1)

-- problem 18
-- λ> slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
-- "cdefg"
slice :: [a] -> Int -> Int -> [a]
slice xs f t | f > 0 = take (t - f + 1) . drop (f - 1) $ xs
slice _ _ _ = error "Invalid parameters"

-- problem 19
-- λ> rotate ['a','b','c','d','e','f','g','h'] 3
-- "defghabc"

-- λ> rotate ['a','b','c','d','e','f','g','h'] (-2)
-- "ghabcdef"
rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs 0 = xs
rotate xs n = drop n' xs ++ take n' xs
  where n' = n `mod` length xs

-- problem 20
-- λ> removeAt 2 "abcd"
-- ('b',"acd")
removeAt :: Int -> [a] -> (a,[a])
removeAt 0 _ = error "Invalid arguments"
removeAt 1 (x:xs) = (x,xs)
removeAt _ [] = error "Invalid arguments"
removeAt n (x:xs) = (y, x:ys)
  where (y,ys) = removeAt (n-1) xs