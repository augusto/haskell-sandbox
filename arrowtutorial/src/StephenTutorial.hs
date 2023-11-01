-- Arrow tutorial from https://en.wikibooks.org/wiki/Haskell/Arrow_tutorial
{-# LANGUAGE Arrows #-}
{-# LANGUAGE ImportQualifiedPost #-}

module StephenTutorial
  ( Circuit (..),
    runCircuit,
    runCircuit',
    accum,
    accum',
    total,
    mean1,
    mean2,
  )
where

import Control.Arrow
import Control.Category qualified as Cat
import Control.Monad
import Data.List
import Data.Maybe
import System.Random

newtype Circuit a b = Circuit {unCircuit :: a -> (Circuit a b, b)}

instance Cat.Category Circuit where
  id = Circuit $ \a -> (Cat.id, a)
  (.) = dot
    where
      (Circuit cir2) `dot` (Circuit cir1) = Circuit $ \a ->
        let (cir1', b) = cir1 a
            (cir2', c) = cir2 b
         in (cir2' `dot` cir1', c)

instance Arrow Circuit where
  arr f = Circuit $ \a -> (arr f, f a)
  first (Circuit cir) = Circuit $ \(b, d) ->
    let (cir', c) = cir b
     in (first cir', (c, d))

runCircuit :: Circuit a b -> [a] -> [b]
runCircuit _ [] = []
runCircuit cir (x : xs) =
  let (cir', x') = unCircuit cir x
   in x' : runCircuit cir' xs

-- or using mapAccumL
runCircuit' :: Circuit a b -> [a] -> [b]
runCircuit' cir inputs =
  snd $ mapAccumL (\cir x -> unCircuit cir x) cir inputs

-- or
-- runCircuit cir = snd . mapAccumL unCircuit cir

-- | Accumulator that outputs a value determined by the supplied function.
accum :: acc -> (a -> acc -> (b, acc)) -> Circuit a b
accum acc f = Circuit $ \a ->
  let (b, acc') = f a acc
   in (accum acc' f, b)

-- | Accumulator that outputs the accumulator value.
accum' :: b -> (a -> b -> b) -> Circuit a b
accum' acc f = accum acc (\a b -> let b' = a `f` b in (b', b'))

total :: (Num a) => Circuit a a
total = accum' 0 (+)

-- Arrow `proc` notation
mean1 :: (Fractional a) => Circuit a a
mean1 = (total &&& (const 1 ^>> total)) >>> arr (uncurry (/))

-- Here is the same function, but written using arrow proc notation:
mean2 :: (Fractional a) => Circuit a a
mean2 = proc value -> do
  t <- total -< value
  n <- total -< 1
  returnA -< t / n

-- Advanced Stuff
mean3 :: (Fractional a) => Circuit a a
mean3 = proc value -> do
  (t, n) <- (| (&&&) (total -< value) (total -< 1) |)
  returnA -< t / n

mean4 :: (Fractional a) => Circuit a a
mean4 = proc value -> do
  (t, n) <- (total -< value) &&& (total -< 1)
  returnA -< t / n

delay :: a -> Circuit a a
delay last = Circuit $ \this -> (delay this, last)

instance ArrowLoop Circuit where
  loop (Circuit cir) = Circuit $ \b ->
    let (cir', (c, d)) = cir (b, d)
     in (loop cir', c)

mean5 :: (Fractional a) => Circuit a a
mean5 = proc value -> do
  rec (lastTot, lastN) <- delay (0, 0) -< (tot, n)
      let (tot, n) = (lastTot + value, lastN + 1)
      let mean = tot / n
  returnA -< mean