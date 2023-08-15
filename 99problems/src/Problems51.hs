module Problems51 where

data Tree a = Empty | Branch a (Tree a) (Tree a)
  deriving (Show, Eq)

-- No problem 51-54

-- Problem 55
-- Construct completely balanced binary trees.
--
-- λ> cbalTree 4
-- [...]
cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n =
  [ Branch 'x' left right | l <- [next .. next + mod], left <- cbalTree l, right <- cbalTree (n - l - 1)
  ]
  where
    (next, mod) = (n - 1) `divMod` 2

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
add n Empty = Branch n Empty Empty
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