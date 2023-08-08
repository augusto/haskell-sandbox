{-# LANGUAGE  ImportQualifiedPost #-}
module Problems21 where

import Data.Functor ((<&>))
import Data.IntSet qualified as IS (IntSet, delete, fromDistinctAscList, size, toAscList)
import Data.List (sortOn)
import Data.List qualified as L (nub, tails, (\\))
import System.Random (Random (randomRs), newStdGen, randomRIO)


-- problem 21
--
-- λ> insertAt 'X' "abcd" 2
-- "aXbcd"
insertAt :: a -> [a] -> Int -> [a]
insertAt _ [] _ = error "Invalid arguments"
insertAt _ _ 0 = error "Invalid arguments"
insertAt x ys 1 = x : ys
insertAt x (y : ys) n = y : insertAt x ys (n - 1)

-- problem 22
--
-- λ> range 4 9
-- [4,5,6,7,8,9]
range :: (Enum a, Ord a) => a -> a -> [a]
range x y
  | x <= y = x : range (succ x) y
  | otherwise = []

-- problem 23
--
-- λ> rnd_select "abcdefgh" 3 >>= putStrLn
-- eda
--
-- recursive solution
rnd_select :: [a] -> Int -> IO [a]
rnd_select [] _ = return []
rnd_select _ 0 = return []
rnd_select xs n = do
  r <- randomRIO (0, (length xs) - 1)
  fmap (xs !! r :) $ rnd_select xs (n - 1)

-- mapping a monadic function
rnd_select1 :: [a] -> Int -> IO [a]
rnd_select1 xs n = mapM (\_ -> rnd_from xs) [1 .. n]

-- using traverse (traverse and mapM have the same structure. Traverse works for Applicative and mapM for monad.
rnd_select2 :: [a] -> Int -> IO [a]
rnd_select2 xs n = traverse (\_ -> rnd_from xs) [1 .. n]

rnd_from :: [a] -> IO a
rnd_from xs = do
  r <- randomRIO (0, length xs - 1)
  return $ xs !! r

rnd_select3 :: [a] -> Int -> IO [a]
rnd_select3 xs c = do
  pos <- sequence $ replicate c (randomRIO (0, length xs - 1))
  return [xs !! p | p <- pos]

-- Generate an infinite list of bounded random numbers and use that as indexes.
rnd_select4 :: [a] -> Int -> IO [a]
rnd_select4 xs n = do
  randoms <- randomRsIO (0, length xs - 1)
  return $ take n [xs !! i | i <- randoms]

randomRsIO :: (Random a) => (a, a) -> IO [a]
randomRsIO range' = newStdGen <&> randomRs range'

-- Problem 24
-- Lotto: Draw N different random numbers from the set 1..M.
--
-- λ> diff_select 6 49
-- [23,1,17,33,21,37]
--
-- This works ok for N < M and small M.
-- when N=M=10000 this implementation is slow and takes~ 5 secs to run.
diff_select :: Int -> Int -> IO [Int]
diff_select n m
  | n <= m = take n . L.nub <$> randomRsIO (1, m)
  | otherwise = return []

-- implement this in a different way which performs ok for large N and M.
-- Recursive implementation which removes the selected item.
-- Uses an IntSet for 'speedy deletes', but needs to transform to a list in order
-- to get the Nth element. This takes ~0.8s for n=m=10000
diff_select1 :: Int -> Int -> IO [Int]
diff_select1 n m | n > m = return []
diff_select1 n m = go n (IS.fromDistinctAscList [1 .. m])
  where
    go :: Int -> IS.IntSet -> IO [Int]
    go 0 _ = return []
    go n' ms' = do
      let maxIdx = IS.size ms' - 1
      idx <- randomRIO (0, maxIdx)
      let e = IS.toAscList ms' !! idx
      rest <- go (n' - 1) (IS.delete e ms')
      return $ e : rest

-- Slightly modified version from the haskell wiki. the list operations make it
-- quite slow 5.5s for n=m=10000.
diff_select2 :: Int -> Int -> IO [Int]
diff_select2 n to = go n [1 .. to]
  where
    go 0 _ = return []
    go _ [] = error "too few elements to choose from"
    go n xs = do
      r <- randomRIO (0, (length xs) - 1)
      let remaining = take r xs ++ drop (r + 1) xs
      rest <- go (n - 1) remaining
      return ((xs !! r) : rest)

-- Problem 25
-- Generate a random permutation of the elements of a list.
--
-- λ> rnd_permu "abcdef"
-- "badcef"
--
-- Uses the solution from problem 24 (problem 23 returned duplicates)
rnd_permu :: [a] -> IO [a]
rnd_permu xs = do
  rs <- diff_select1 len len
  return [xs !! (i - 1) | i <- rs]
  where
    len = length xs

-- problem 26
-- Generate combinations of K distinct objects chosen from the N elements of a list.
--
-- In how many ways can a committee of 3 be chosen from a group of 12 people? We all
-- know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known
-- binomial coefficients). For pure mathematicians, this result may be great. But we
-- want to really generate all the possibilities in a list.
--
-- Example:
-- λ> combinations 3 "abcdef"
-- ["abc","abd","abe",...]
--
-- This solution is too complicated. It needs the first match to return an empty list
--  in the scenario in which n > length xs. Interstingly, it's similar to a solution in
-- the haskell wiki
combinations :: Int -> [a] -> [[a]]
combinations _ [] = []
combinations 1 xs = [[x] | x <- xs]
combinations n (x : xs) = [x : rest | rest <- combinations (n - 1) xs] ++ combinations n xs

-- From the haskell wiki, using tails
combinations1 :: Int -> [a] -> [[a]]
combinations1 0 _ = [[]]
combinations1 n xs = [y : rs | (y : ys) <- L.tails xs, rs <- combinations1 (n - 1) ys]

-- Problem 27
-- Group the elements of a set into disjoint subsets
-- a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3
-- and 4 persons? Write a function that generates all the possibilities and returns
-- them in a list.
-- b) Generalize the above predicate in a way that we can specify a list of group
--  sizes and the predicate will return a list of groups
--
-- Note that we do not want permutations of the group members; i.e. ((ALDO BEAT) ...)
-- is the same solution as ((BEAT ALDO) ...). However, we make a difference between
-- ((ALDO BEAT) (CARLA DAVID) ...) and ((CARLA DAVID) (ALDO BEAT) ...).
--
-- You may find more about this combinatorial problem in a good book on discrete
--  mathematics under the term "multinomial coefficients".
--
-- λ> group [2,3,4] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
-- [[["aldo","beat"],["carla","david","evi"],["flip","gary","hugo","ida"]],...]
-- (altogether 1260 solutions)

-- λ> group [2,2,5] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
-- [[["aldo","beat"],["carla","david"],["evi","flip","gary","hugo","ida"]],...]
-- (altogether 756 solutions)
group :: Eq a => [Int] -> [a] -> [[[a]]]
group gs xs
  | sum gs /= length xs = []
  | length gs == 1 = [[xs]]
  | otherwise =
      [ ys : rest
        | (g' : gs') <- L.tails gs,
          ys <- combinations g' xs,
          rest <- group gs' (xs L.\\ ys)
      ]

-- From the haskell Wiki
-- helper function
combinationHW :: Int -> [a] -> [([a], [a])]
combinationHW 0 xs = [([], xs)]
combinationHW n [] = []
combinationHW n (x : xs) = ts ++ ds
  where
    ts = [(x : ys, zs) | (ys, zs) <- combinationHW (n - 1) xs]
    ds = [(ys, x : zs) | (ys, zs) <- combinationHW n xs]

group1 :: [Int] -> [a] -> [[[a]]]
group1 [] = const [[]]
group1 (n : ns) = concatMap (uncurry $ (. group1 ns) . map . (:)) . combinationHW n

-- Same as above but split in chunks
group2 :: [Int] -> [a] -> [[[a]]]
group2 [] = const [[]]
group2 (n : ns) = concatMap mapfn . combinationHW n
  where
    mapfn :: ([a], [a]) -> [[[a]]]
    mapfn = (uncurry $ fn)
    fn :: [a] -> [a] -> [[[a]]]
    fn = (. group2 ns) . map . (:)

-- Problem 28
-- Sorting a list of lists according to length of sublists.
-- We suppose that a list contains elements that are lists themselves. The objective is to sort the elements
-- of this list according to their length. E.g. short lists first, longer lists later, or vice versa.
--
-- λ> lsort ["abc","de","fgh","de","ijkl","mn","o"]
-- ["o","de","de","mn","abc","fgh","ijkl"]

-- cheap and cheerful
lsort :: [[a]] -> [[a]]
lsort = sortOn length

-- a potential faster implementation that calculates the length once
lsort1 :: [[a]] -> [[a]]
lsort1 = map snd . sortOn fst . map (\xs -> (length xs, xs))

-- Problem 29 / 30
-- There is no problem 29 or 30