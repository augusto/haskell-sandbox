module Sudoku
  ( solve4,
    diabolical,
    printGrid
  )
where

import Data.List

type Value = Char

type Row a = [a]

type Matrix a = [Row a]

type Grid = Matrix Value

boxsize :: Int
boxsize = 3

values :: [Value]
values = ['1' .. '9']

empty :: Value -> Bool
empty = (== '.')

-- Example Grids

-- Solvable only using the basic rules:
easy :: Grid
easy =
  [ "2....1.38",
    "........5",
    ".7...6...",
    ".......13",
    ".981..257",
    "31....8..",
    "9..8...2.",
    ".5..69784",
    "4..25...."
  ]

-- First gentle example from sudoku.org.uk:

gentle :: Grid
gentle =
  [ ".1.42...5",
    "..2.71.39",
    ".......4.",
    "2.71....6",
    "....4....",
    "6....74.3",
    ".7.......",
    "12.73.5..",
    "3...82.7."
  ]

-- First diabolical example:

diabolical :: Grid
diabolical =
  [ ".9.7..86.",
    ".31..5.2.",
    "8.6......",
    "..7.5...6",
    "...3.7...",
    "5...1.7..",
    "......1.9",
    ".2.6..35.",
    ".54..8.7."
  ]

-- First "unsolvable" (requires backtracking) example:

unsolvable :: Grid
unsolvable =
  [ "1..9.7..3",
    ".8.....7.",
    "..9...6..",
    "..72.94..",
    "41.....95",
    "..85.43..",
    "..3...7..",
    ".5.....4.",
    "2..8.6..9"
  ]

-- Minimal sized grid (17 values) with a unique solution:

minimal :: Grid
minimal =
  [ ".98......",
    "....7....",
    "....15...",
    "1........",
    "...2....9",
    "...9.6.82",
    ".......3.",
    "5.1......",
    "...4...2."
  ]

-- Empty grid:

blank :: Grid
blank = replicate n (replicate n '.')
  where
    n = boxsize ^ 2

test :: Grid
test =
  [ "123456789",
    "123456789",
    "123456789",
    "123456789",
    "123456789",
    "123456789",
    "123456789",
    "123456789",
    "123456789"
  ]

printGrid :: Grid -> IO ()
printGrid = putStr . unlines

-- Extracting rows, columns and boxes
-------------------------------------
rows :: Matrix a -> [Row a]
rows x = x

-- Hand made rather than use transpose
cols :: Matrix a -> [Row a]
cols xs = trans 0
  where
    -- TODO write this as a list comprehension
    trans row
      | row >= length xs = []
      | otherwise = map (!! row) xs : trans (row + 1)

boxs :: Matrix a -> [Row a]
boxs xs = map boxify coords
  where
    coords = [(x, y) | y <- [0 .. boxsize - 1], x <- [0 .. boxsize - 1]]
    boxify (x, y) =
      [ val
        | rowNum <- [y * boxsize .. (y + 1) * boxsize - 1],
          val <- take boxsize . drop (x * boxsize) $ xs !! rowNum
      ]

-- Validity checking
--------------------

valid :: Grid -> Bool
valid g =
  all nodups (rows g)
    && all nodups (cols g)
    && all nodups (boxs g)

nodups :: Eq a => [a] -> Bool
nodups [] = True
nodups (x : xs) = notElem x xs && nodups xs

--
-- A basic solver (brute force)
-------------------------------
type Choices = [Value]

choices :: Grid -> Matrix Choices
choices = map mapRow
  where
    mapRow = map (\i -> if empty i then values else [i])

collapse :: Matrix Choices -> [Grid]
collapse = cp . map cp

cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs : xss) = [y : ys | y <- xs, ys <- cp xss]

solve1 :: Grid -> [Grid]
solve1 = filter valid . collapse . choices

--
-- A more advanced solver which reduce the number of choices in a given row/column/box
--------------------------------------------------------------------------------------
prune :: Matrix Choices -> Matrix Choices
prune = pruneBy boxs . pruneBy cols . pruneBy rows

pruneBy :: (Matrix Choices -> [Row Choices]) -> Matrix Choices -> Matrix Choices
pruneBy f = f . map pruneChoices . f

pruneChoices :: Row Choices -> Row Choices
pruneChoices xs = foldr removeChoices xs [0 .. length values - 1]
  where
    removeChoices i acc = case acc !! i of
      [e] -> map (delete e) (take i acc) ++ ([e] : map (delete e) (drop (i + 1) acc))
      _ -> acc

solve2 :: Grid -> [Grid]
solve2 = filter valid . collapse . prune . choices

fix :: (Eq a) => (a -> a) -> a -> a
fix f x = if x == next then x else fix f next
  where
    next = f x

solve3 :: Grid -> [Grid]
solve3 = filter valid . collapse . fix prune . choices

--
-- Even more advanced solution
--
--

void :: Matrix Choices -> Bool
void = any (any null)

safe :: Matrix Choices -> Bool
safe m =
  all consistent (rows m)
    && all consistent (cols m)
    && all consistent (boxs m)

consistent :: Row Choices -> Bool
consistent r = go r []
  where
    go :: Row Choices -> Choices -> Bool
    go [] _ = True
    go ([x] : xs) is = notElem x is && go xs (x : is)
    go (_ : xs) is = go xs is

blocked :: Matrix Choices -> Bool
blocked m = void m || not (safe m)

search :: Matrix Choices -> [Grid]
search m
  | blocked m = []
  | all (all single) m = collapse m
  | otherwise =
      [ g | m' <- expand m, g <- search (prune m')
      ]

expand :: Matrix Choices -> [Matrix Choices]
expand m =
  [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs]
  where
    (rows1, row : rows2) = break (any (not . single)) m
    (row1, cs : row2) = break (not . single) row

single :: Choices -> Bool
single [_] = True
single _ = False

solve4 :: Grid -> [Grid]
solve4 = search . prune . choices
