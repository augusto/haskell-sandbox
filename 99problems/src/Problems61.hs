module Problems61 where

import Debug.Trace (trace)
import Problems51 (Tree (Branch, Empty), leaf, treeSize)
import Text.Printf (printf)

tree4 :: Tree Integer
tree4 =
  Branch
    1
    (Branch 2 Empty (Branch 4 Empty Empty))
    (Branch 2 Empty Empty)

-- Problem 61
-- Count the leaves of a binary tree. Solutions
--
-- A leaf is a node with no successors. Write a predicate count_leaves/2 to count them.
--
-- λ> countLeaves tree4
-- 2
countLeaves :: Tree a -> Int
countLeaves Empty = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ lb rb) = countLeaves lb + countLeaves rb

-- Problem 61A
-- Collect the leaves of a binary tree in a list. Solutions
--
-- A leaf is a node with no successors. Write a predicate leaves/2 to collect them in a list.
--
-- λ> leaves tree4
-- [4,2]
leaves :: Tree a -> [a]
leaves Empty = []
leaves (Branch x Empty Empty) = [x]
leaves (Branch _ lt rt) = leaves lt ++ leaves rt

-- Problem 62
-- Collect the internal nodes of a binary tree in a list. Solutions
--
-- An internal node of a binary tree has either one or two non-empty successors. Write a predicate internals/2 to collect them in a list.
--
-- λ> internals tree4
-- [1,2]
internals :: Tree a -> [a]
internals Empty = []
internals (Branch _ Empty Empty) = []
internals (Branch x lt rt) = x : internals lt ++ internals rt

-- Problem 62B
-- Collect the nodes at a given level in a list. Solutions
--
-- A node of a binary tree is at level N if the path from the root to the node has length N-1.
-- The root node is at level 1. Write a predicate atlevel/3 to collect all nodes at a given
--  level in a list.
--
-- λ> atLevel tree4 2
-- [2,2]
atLevel :: Tree a -> Int -> [a]
atLevel Empty _ = []
atLevel (Branch x _ _) 1 = [x]
atLevel (Branch _ lt rt) n =
  let nextN = n - 1
   in atLevel lt nextN ++ atLevel rt nextN

-- Problem 63
-- Construct a complete binary tree. Solutions
--
-- A complete binary tree with height H is defined as follows:
--   * The levels 1,2,3,...,H-1 contain the maximum number of nodes (i.e 2**(i-1) at the level i)
--   * In level H, which may contain less than the maximum possible number of nodes, all
--     the nodes are "left-adjusted". This means that in a levelorder tree traversal all
--     internal nodes come first, the leaves come second, and empty successors (the nil's
--     which are not really nodes!) come last.
--
-- Particularly, complete binary trees are used as data structures (or addressing schemes)
--  for heaps.
--
-- We can assign an address number to each node in a complete binary tree by enumerating
-- the nodes in level-order, starting at the root with number 1. For every node X with
-- address A the following property holds: The address of X's left and right successors
-- are 2*A and 2*A+1, respectively, if they exist. This fact can be used to elegantly
-- construct a complete binary tree structure.
--
-- λ> completeBinaryTree 4
-- Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty Empty)
--
-- λ> isCompleteBinaryTree $ Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)
-- True
completeBinaryTree :: Int -> Tree Char
completeBinaryTree n = go 1
  where
    go a =
      if a <= n
        then Branch 'x' (go (2 * a)) (go (2 * a + 1))
        else Empty

completeTree :: Tree Integer
completeTree =
  Branch
    1
    (Branch 2 (leaf 3) (leaf 3))
    (Branch 2 Empty Empty)

nonCompleteTree :: Tree Integer
nonCompleteTree =
  Branch
    1
    (Branch 2 (leaf 3) Empty)
    (Branch 2 Empty (leaf 3))

nonCompleteTree2 :: Tree Integer
nonCompleteTree2 =
  Branch
    1
    (Branch 2 (leaf 3) (leaf 3))
    (Branch 2 Empty (leaf 3))

-- Bit of a messy implementation
-- First checks the 'depth' going through the left; then checks all branches have
-- that length or one less. When the depth is one less than the left depth, the
-- jump flag and from there on all branches must be ldepth-1
isCompleteBinaryTree :: Tree a -> Bool
isCompleteBinaryTree Empty = True
isCompleteBinaryTree (Branch _ Empty Empty) = True
isCompleteBinaryTree x = fst $ eval 1 False x
  where
    -- Params: Current Depth, jump, tree -> (isComplete, jump)
    maxdepth = ldeep x
    eval :: Int -> Bool -> Tree a -> (Bool, Bool)
    eval cd False Empty | maxdepth + 1 == cd = (True, False)
    eval cd False Empty | maxdepth == cd = (True, True)
    eval _ False Empty = (False, False)
    eval cd True Empty = (maxdepth == cd, True)
    eval cd breach (Branch _ lt rt) =
      let (isComplete, hasBreach) = eval (cd + 1) breach lt
       in if not isComplete
            then (False, hasBreach)
            else eval (cd + 1) hasBreach rt

ldeep :: Tree a -> Int
ldeep Empty = 0
ldeep (Branch _ lt _) = 1 + ldeep lt