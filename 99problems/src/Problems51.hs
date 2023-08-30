module Problems51(
  Tree(Empty,Branch),
  depth,
  treeSize,
  leaf,
  pt
) where

data Tree a = Empty | Branch a (Tree a) (Tree a)
  deriving (Show, Eq)

-- Some utilities

depth :: Tree a -> Int
depth Empty = 0
depth (Branch _ l r) = 1 + max (depth l) (depth r)

treeSize :: Tree a -> Int
treeSize Empty = 0
treeSize (Branch _ l r) = 1 + treeSize l + treeSize r

leaf :: a -> Tree a
leaf x = Branch x Empty Empty

pt :: Show a => [Tree a] -> IO ()
pt = putStrLn . unlines . map show

-- No problem 51-54

-- Problem 55
-- Construct completely balanced binary trees.
--
-- λ> cbalTree 4
-- [...]
cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n = cbalTree' n 'x'

cbalTree' :: Int -> a -> [Tree a]
cbalTree' 0 _ = [Empty]
cbalTree' n x = [Branch x left right | l <- [next .. next + mod], left <- cbalTree' l x, right <- cbalTree' (n - l - 1) x]
  where
    (next, mod) = (n - 1) `divMod` 2

-- or another approach using later functions
cbalTree2 :: Int -> [Tree Char]
cbalTree2 n = [t | t <- genTree n, isCbal t]

-- Problem 56
-- Symmetric binary trees.
--
-- Let us call a binary tree symmetric if you can draw a vertical line through the
-- root node and then the right subtree is the mirror image of the left subtree.
-- Write a predicate symmetric/1 to check whether a given binary tree is symmetric.
-- Hint: Write a predicate mirror/2 first to check whether one tree is the mirror
-- image of another. We are only interested in the structure, not in the contents
-- of the nodes.
--
-- λ> symmetric (Branch 'x' (Branch 'x' Empty Empty) Empty)
-- False
-- λ> symmetric (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty))
-- True
symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ l r) = symmetric' l r
  where
    symmetric' :: Tree a -> Tree a -> Bool
    symmetric' Empty Empty = True
    symmetric' (Branch _ l1 r1) (Branch _ l2 r2) = symmetric' l1 r2 && symmetric' r1 l2
    symmetric' _ _ = False

-- Problem 57
-- Binary search trees.
--
-- Use the predicate add/3, developed in chapter 4 of the course, to write a predicate
-- to construct a binary search tree from a list of integer numbers.
--
-- λ> construct [3, 2, 5, 7, 1]
-- Branch 3 (Branch 2 (Branch 1 Empty Empty) Empty) (Branch 5 Empty (Branch 7 Empty Empty))
-- λ> symmetric . construct $ [5, 3, 18, 1, 4, 12, 21]
-- True
-- λ> symmetric . construct $ [3, 2, 5, 7, 1]
-- True
--
-- Node: No idea what chapter 4 is, but it looks like it's just to create an 'add' function that
-- takes an Int and inserts/adds it to a tree.
add :: Int -> Tree Int -> Tree Int
add n Empty = leaf n
add n (Branch e lt rt)
  | n < e = Branch e (add n lt) rt
  | e < n = Branch e lt (add n rt)
  | otherwise = Branch e lt rt

construct :: [Int] -> Tree Int
construct = foldl (flip add) Empty

-- Problem 58
-- Generate-and-test paradigm.
--
-- Apply the generate-and-test paradigm to construct all symmetric, completely balanced
-- binary trees with a given number of nodes.
--
-- λ> symCbalTrees 5
-- [Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' (Branch 'x' Empty Empty) Empty),Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty (Branch 'x' Empty Empty))]
--
--
symCbalTrees n = [t | t <- cbalTree n, symmetric t]

symCbalTrees'' = filter symmetric . cbalTree

-- Implementation that builds all possible trees with n nodes and then filters
-- those which are not CBAL
genTree :: Int -> [Tree Char]
genTree 0 = [Empty]
genTree n =
  [ Branch 'x' lt rt | r <- [0 .. (n - 1)], lt <- genTree (n - r - 1), rt <- genTree r
  ]

isCbal :: Tree a -> Bool
isCbal = snd . isCbal'
  where
    isCbal' :: Tree a -> (Int, Bool)
    isCbal' Empty = (0, True)
    isCbal' (Branch _ l' r') =
      let (dl, cl) = isCbal' l'
          (dr, cr) = isCbal' r'
       in -- short circuit if left is not CBAL.
          if cl && cr then (1 + max dl dr, abs (dl - dr) <= 1) else (0, False)

symCbalTrees' :: Int -> [Tree Char]
symCbalTrees' n = [t | t <- genTree n, isCbal t && symmetric t]

-- Problem 59
-- Construct height-balanced binary trees.
--
-- In a height-balanced binary tree, the following property holds for every node:
-- The height of its left subtree and the height of its right subtree are almost
-- equal, which means their difference is not greater than one.
--
-- Construct a list of all height-balanced binary trees with the given element
-- and the given maximum height.
--
-- λ> take 4 $ hbalTree 'x' 3
-- [Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty (Branch 'x' Empty Empty)),
--  Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' (Branch 'x' Empty Empty) Empty),
--  Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)),
--  Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' Empty Empty)]

hbalTree :: a -> Int -> [Tree a]
hbalTree _ 0 = [Empty]
hbalTree x 1 = [leaf x]
hbalTree x n =
  let depths = [(n - 1, n - 2), (n - 2, n - 1), (n - 1, n - 1)]
   in [Branch x lb rb | (ld, rd) <- depths, lb <- hbalTree x ld, rb <- hbalTree x rd]


-- Problem 60
-- Construct height-balanced binary trees with a given number of nodes. 
--
-- Consider a height-balanced binary tree of height H. What is the maximum
--  number of nodes it can contain?
-- 
-- Clearly, MaxN = 2H - 1. However, what is the minimum number MinN? This
--  question is more difficult. Try to find a recursive statement and turn
--  it into a function minNodes that returns the minimum number of nodes in
--  a height-balanced binary tree of height H.
-- 
-- On the other hand, we might ask: what is the maximum height H a height-balanced
--  binary tree with N nodes can have? Write a function maxHeight that computes this.
-- 
-- Now, we can attack the main problem: construct all the height-balanced binary
--  trees with a given number of nodes. Find out how many height-balanced trees
--  exist for N = 15. 
--
-- λ> length $ hbalTreeNodes 'x' 15
-- 1553
-- λ> map (hbalTreeNodes 'x') [0..3]
-- [[Empty],
-- [Branch 'x' Empty Empty],
-- [Branch 'x' Empty (Branch 'x' Empty Empty),Branch 'x' (Branch 'x' Empty Empty) Empty],
-- [Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)]]
maxN :: Int -> Int
maxN h = 2^h - 1

minN :: Int -> Int
minN 0 = 0
minN 1 = 1
minN h = 1 + minN (h-1) + minN (h-2)

hbalTreeNodes :: a -> Int -> [Tree a]
hbalTreeNodes _ 0 = [Empty]
hbalTreeNodes x 1 = [leaf x]
hbalTreeNodes x n = let minNodes = minN (n-1)
                        maxNodes = maxN 
                    in undefined
                    