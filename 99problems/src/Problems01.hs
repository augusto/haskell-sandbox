module Problems01 where

-- problem 5
-- λ> myReverse "A man, a plan, a canal, panama!"
-- "!amanap ,lanac a ,nalp a ,nam A"
-- λ> myReverse [1,2,3,4]
-- [4,3,2,1]
myReverse :: [a] -> [a]
myReverse [] = []
myReverse xs = foldl (flip (:)) [] xs

-- Problem 6
-- λ> isPalindrome [1,2,3]
-- False
-- λ> isPalindrome "madamimadam"
-- True
-- λ> isPalindrome [1,2,4,8,16,8,4,2,1]
-- True
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

-- problem 7
-- λ> flatten (Elem 5)
-- [5]
-- λ> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
-- [1,2,3,4,5]
-- λ> flatten (List [])
-- []
data NestedList a = Elem a | List [NestedList a] deriving (Show)

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List xs) = concatMap flatten xs

-- problem 8
-- λ> compress "aaaabccaadeeee"
-- "abcade"
compress :: Eq a => [a] -> [a]
compress [] = []
compress (x : xs) = x : compress (dropWhile (x ==) xs)

-- copied from the solutions
compress2 :: Eq a => [a] -> [a]
compress2 (x : xs@(y : _))
  | x == y = compress2 xs
  | otherwise = x : compress2 xs
compress2 xs = xs

-- problem 9
-- λ> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a','a', 'd', 'e', 'e', 'e', 'e']
-- ["aaaa","b","cc","aa","d","eeee"]
pack :: Eq a => [a] -> [[a]]
pack [] = [[]]
pack (x : xs) = go [x] xs
  where
    go acc [] = [acc]
    go acc yss@(y : ys)
      | head acc == y = go (y : acc) ys
      | otherwise = acc : pack yss

-- problem 10
-- λ> encode "aaaabccaadeeee"
-- [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode xs = map (\i -> (length i, head i)) . pack $ xs


encode2 :: Eq a => [a] -> [(Int, a)]
encode2 [] = []
encode2 (x:xs) = go (1,x) xs 
  where go acc [] = [acc]
        go (i,y) (z:zs)
          | y == z = go (i+1,y) zs
          | otherwise = (i,y) : encode2 (z:zs)