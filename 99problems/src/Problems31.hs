module Problems31
  ( goldbach
  )
where

import Data.List

-- problem 31
-- λ> isPrime 7
-- True
-- λ> take 20 ([y|y<-[1..], isPrime y])
-- [1,2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67]
isPrime :: Int -> Bool
isPrime x
  | x == 1 || x == 2 || x == 3 = True
  | even x = False
  | otherwise = isPrime' 3 (floor . sqrt . fromIntegral $ x)
  where
    isPrime' :: Int -> Int -> Bool
    isPrime' y sqr
      | y > sqr = True
      | x `mod` y == 0 = False
      | otherwise = isPrime' (y + 2) sqr

-- problem 32
-- λ> [myGCD 36 63, myGCD (-3) (-6), myGCD (-3) 6]
-- [9,3,3]
mygcd :: Int -> Int -> Int
mygcd a 0 = abs a
mygcd a b = mygcd b (a `mod` b)

mygcd' :: Int -> Int -> Int
mygcd' a b
  | a < 0 = mygcd' (-a) b
  | b < 0 = mygcd' a (-b)
  | a == b = a
  | a > b = mygcd' (a - b) b
  | a < b = mygcd' a (b - a)

-- problem 33
-- λ> coprime 35 64
-- True
coprime :: Int -> Int -> Bool
coprime a b = gcd a b == 1

-- problem 34
-- λ> totient 10
-- 4
totient :: Int -> Int
totient 1 = 1
totient x = length [c | c <- [1 .. (x - 1)], coprime c x]

-- problem 35
-- λ> primeFactors 315
-- [3, 3, 5, 7]
primeFactors :: Int -> [Int]
primeFactors 1 = []
primeFactors x = go 2 (floor . sqrt . fromIntegral $ x) x
  where
    go a b y
      | a > b = [y]
      | y `mod` a == 0 = a : primeFactors (y `div` a)
      | otherwise = go (a + 1) b y

-- problem 36
-- λ> prime_factors_mult 315
-- [(3,2),(5,1),(7,1)]
prime_factors_mult :: Int -> [(Int, Int)]
prime_factors_mult = map mapping . group . primeFactors
  where
    mapping xs@(x : _) = (x, length xs)

-- problem 37
-- See problem 34 for the definition of Euler's totient function. If the list of the prime
-- factors of a number m is known in the form of problem 36 then the function phi(m) can be
-- efficiently calculated as follows: Let ((p1 m1) (p2 m2) (p3 m3) ...) be the list of prime
-- factors (and their multiplicities) of a given number m. Then phi(m) can be calculated
-- with the following formula:

-- phi(m) = (p1 - 1) * p1 ** (m1 - 1) *
--          (p2 - 1) * p2 ** (m2 - 1) *
--          (p3 - 1) * p3 ** (m3 - 1) * ...
totient' :: Int -> Int
totient' x = go (prime_factors_mult x)
  where
    go [] = 1
    go ((p, m) : xs) = ((p - 1) * p ^ (m - 1)) * (go xs)

-- haskell wiki solution <3
-- totient m = product [(p - 1) * p ^ (c - 1) | (p, c) <- prime_factors_mult m]

-- problem 38
-- No solutions

-- problem 39
-- λ> primesR 10 20
-- [11,13,17,19]
primesR :: Int -> Int -> [Int]
primesR x y = [p | p <- [x .. y], isPrime p]

-- problem 40
-- Goldbach's conjecture says that every positive even number greater than 2 is the sum
-- of two prime numbers. Example: 28 = 5 + 23. It is one of the most famous facts in
-- number theory that has not been proved to be correct in the general case. It has been
-- numerically confirmed up to very large numbers (much larger than we can go with our
-- Prolog system). Write a predicate to find the two prime numbers that sum up to a given
-- even integer.
--
-- λ> goldbach 28
-- (5, 23)
goldbach :: Int -> [(Int, Int)]
goldbach n = [(p1, p2) | p1 <- primes, p2 <- primes, p1 + p2 == n]
  where
    primes = primesR 2 n
