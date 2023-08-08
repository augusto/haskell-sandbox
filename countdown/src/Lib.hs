module Lib
  ( findsols,
  findsols'
  )
where

import Data.List

data Op = Add | Sub | Mul | Div deriving (Show)

apply :: Op -> Int -> Int -> Int
apply Add = (+)
apply Sub = (-)
apply Mul = (*)
apply Div = div

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = y /= 0 && x `mod` y == 0 

data Expr = Val Int | App Op Expr Expr deriving (Show)

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l, y <- eval r, valid o x y]

-- Returns all possible ways of choosing zero or more elements from a list
choices :: [a] -> [[a]]
choices = concatMap permutations' . subsequences'

subsequences' :: [a] -> [[a]]
subsequences' = foldl (\a i -> a ++ map (++ [i]) a) [[]]

permutations' :: [a] -> [[a]]
permutations' [] = [[]]
permutations' [x] = [[x]]
permutations' xs = concatMap combrec (rotations xs)
  where
    combrec (y : ys) = map (y :) (permutations' ys)

rotations :: [a] -> [[a]]
rotations xs = go xs (length xs)
  where
    go _ 0 = []
    go yss@(y : ys) l = yss : go (ys ++ [y]) (l - 1)
    go _ _ = undefined

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ le re) = values le ++ values re

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n =  elem (values e) (choices ns) && eval e == [n] 

split :: [a] -> [([a],[a])]
split (x:y:xs) = go [([x], y:xs)] [x] (y:xs)
  where go acc _ [_] = acc 
        go acc l (r:rs) = acc ++ go [(l++[r], rs)] (l++[r]) rs
split _ = error "Must have at least 2 values"

genexpr :: [Int] -> [Expr]
genexpr [] = []
genexpr [x] = [Val x]
genexpr ns = [ App o le re | o <- [Add,Sub,Mul,Div],
                  (l,r) <- split ns,
                  le <- genexpr l,
                  re <- genexpr r]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [ e | (l,r) <- split ns,
                le <- exprs l,
                re <- exprs r,
                e <- combine le re]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- [Add,Sub,Mul,Div]
                         , lv <- eval l
                         , rv <- eval r
                         , valid o lv rv]


-- ghci> head (findsols [1,3,7,10,25,30] 765)
-- App Sub (App Mul (App Add (Val 1) (Val 30)) (Val 25)) (App Add (Val 3) (Val 7))
-- (8.14 secs, 4,289,355,144 bytes)
-- ghci> length (findsols [1,3,7,10,25,30] 765)
-- 3956
-- (284.63 secs, 153,264,598,808 bytes)
findsols :: [Int] -> Int -> [Expr]
findsols ns n = [e| cs <- choices ns,
                    length cs > 2,
                    e <- genexpr cs,
                    eval e == [n]]

-- ghci> head (findsols' [1,3,7,10,25,30] 765)
-- App Sub (App Mul (App Add (Val 1) (Val 30)) (Val 25)) (App Add (Val 3) (Val 7))
-- (3.19 secs, 1,826,131,768 bytes)
-- ghci> length (findsols' [1,3,7,10,25,30] 765)
-- 3956
-- (98.96 secs, 49,169,195,288 bytes)
findsols' :: [Int] -> Int -> [Expr]
findsols' ns n = [e| cs <- choices ns,
                    length cs > 2,
                    e <- exprs cs,
                    eval e == [n]]