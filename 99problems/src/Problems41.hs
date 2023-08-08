module Problems41 where

import Control.Monad (replicateM)
import Data.List (intersperse)
import Problems31 (goldbach)
import Text.Printf (printf)

-- Problem 41
-- A list of even numbers and their Goldbach compositions in a given range.

-- λ> goldbachList 9 20
-- [(3,7),(5,7),(3,11),(3,13),(5,13),(3,17)]
-- λ> goldbachList' 4 2000 50
-- [(73,919),(61,1321),(67,1789),(61,1867)]
--
-- The 2nd example doesn't make sense
goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList f t = fmap (head . goldbach) [f', f' + 2 .. t]
  where
    f' = f + (f `mod` 2)

-- No Problem 42 / 43 / 44 /45

-- Problem 46
-- Truth tables for logical expressions.
-- Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2
-- (for logical equivalence) which succeed or fail according to the result
-- of their respective operations; e.g. and(A,B) will succeed, if and only if
-- both A and B succeed.
--
-- A logical expression in two variables can then be written as in the
-- following example: and(or(A,B),nand(A,B)).
--
-- Now, write a predicate table/3 which prints the truth table of a given
-- logical expression in two variables.
--
-- λ> table (\a b -> (and' a (or' a b)))
-- True True True
-- True False True
-- False True False
-- False False False
not' :: Bool -> Bool
not' = not

and' :: Bool -> Bool -> Bool
and' = (&&)

or' :: Bool -> Bool -> Bool
or' = (||)

nand' :: Bool -> Bool -> Bool
nand' x y = not' (x `and'` y)

nor' :: Bool -> Bool -> Bool
nor' x y = not' (x `or'` y)

xor' :: Bool -> Bool -> Bool
xor' x y = not' (x `equ'` y)

equ' :: Bool -> Bool -> Bool
equ' = (==)

-- IMPL is True if a implies b, equivalent to (not a) or (b) -- ?????
impl' a b = (not' a) `or'` b

table :: (Bool -> Bool -> Bool) -> IO ()
table f = mapM_ (putStrLn . pretty) vals
  where
    bs = [True, False]
    vals = [(x, y, f x y) | x <- bs, y <- bs]
    pretty (x, y, z) = printf "%-5s %-5s %-5s" (show x) (show y) (show z)

-- Problem 47
-- Continue Problem 46 by defining and/2, or/2, etc as being operators.
-- This allows to write the logical expression in the more natural way,
-- as in the example: A and (A or not B). Define operator precedence as
-- usual; i.e. as in Java.
--
-- λ> table2 (\a b -> a `and'` (a `or'` not b))
-- True True True
-- True False True
-- False True False
-- False False False

infixl 4 `or'`

infixl 6 `and'`

-- Problem 48
--
-- Generalize Problem 47 in such a way that the logical expression may
-- contain any number of logical variables. Define table/2 in a way that
-- table(List,Expr) prints the truth table for the expression Expr, which
-- contains the logical variables enumerated in List.
--
-- λ> tablen 3 (\[a,b,c] -> a `and'` (b `or'` c) `equ'` a `and'` b `or'` a `and'` c)
-- -- infixl 3 `equ'`
-- True  True  True  True
-- True  True  False True
-- True  False True  True
-- True  False False True
-- False True  True  True
-- False True  False True
-- False False True  True
-- False False False True
--
-- -- infixl 7 `equ'`
-- True  True  True  True
-- True  True  False True
-- True  False True  True
-- True  False False False
-- False True  True  False
-- False True  False False
-- False False True  False
-- False False False False
-- infixl 3 `equ'`
-- infixl 7 `equ'`
tablen :: Int -> ([Bool] -> Bool) -> IO ()
tablen n fn = mapM_ (\v -> putStrLn (pretty v (fn v))) (args n)
  where
    pretty vs r = unwords (map show vs) ++ " " ++ show r

args :: Int -> [[Bool]]
args 0 = [[]]
args n = [x : rest | x <- [True, False], rest <- args (n - 1)]

-- much fancier
-- args n = replicateM n [True,False]
-- or
-- args n = sequence (replicate n [True,False])

-- Problem 49
-- Gray Codes
--
-- An n-bit Gray code is a sequence of n-bit strings constructed according to
-- certain rules. For example,
--
-- n = 1: C(1) = ['0','1'].
-- n = 2: C(2) = ['00','01','11','10'].
-- n = 3: C(3) = ['000','001','011','010',´110´,´111´,´101´,´100´].
--
-- Find out the construction rules and write a predicate with the following specification:
--
-- % gray(N,C) :- C is the N-bit Gray code
--
-- Can you apply the method of "result caching" in order to make the predicate
-- more efficient, when it is to be used repeatedly?
--
-- λ> gray 3
-- ["000","001","011","010","110","111","101","100"]
--
-- From https://en.wikipedia.org/wiki/Gray_code#Constructing_an_n-bit_Gray_code
gray :: Int -> [String]
gray 1 = ["0","1"]
gray n = map ('0':) old ++ map ('1':) (reverse old)
  where 
    old = gray (n - 1)


pad :: Int -> Char -> String -> String
pad n c str = replicate len c ++ str
  where
    len = max (n - length str) 0

toBinary :: Int -> String
toBinary 0 = ""
toBinary x = toBinary d ++ show m
  where
    (d, m) = x `divMod` 2