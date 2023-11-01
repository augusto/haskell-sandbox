{-# LANGUAGE Arrows #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Hangman
  ( generator,
    pickWord,
    hangman,
    runHangman
  )
where

import Control.Arrow
import Control.Category qualified as Cat
import Control.Monad
import Data.List
import Data.Maybe
import StephenTutorial
import System.Random

generator :: (Random a) => (a, a) -> StdGen -> Circuit () a
generator range rng = accum rng $ \() rng -> randomR range rng

dictionary = ["dog", "cat", "bird"]

pickWord :: StdGen -> Circuit () String
pickWord rng = proc () -> do
  idx <- generator (0, length dictionary - 1) rng -< ()
  returnA -< dictionary !! idx

oneShot :: Circuit () Bool
oneShot = accum True $ \_ acc -> (acc, False)

delayedEcho :: a -> Circuit a a
delayedEcho acc = accum acc (\a b -> (b, a))

-- or a shorter one
delayedEcho' :: a -> Circuit a a
delayedEcho' acc = accum acc (flip (,))

instance ArrowChoice Circuit where
  left orig@(Circuit cir) = Circuit $ \ebd -> case ebd of
    Left b ->
      let (cir', c) = cir b
       in (left cir', Left c)
    Right d -> (left orig, Right d)

getWord :: StdGen -> Circuit () String
getWord rng = proc () -> do
  -- If this is the first game loop, run pickWord. mPicked becomes Just <word>.
  -- On subsequent loops, mPicked is Nothing.
  firstTime <- oneShot -< ()
  mPicked <-
    if firstTime
      then do
        picked <- pickWord rng -< ()
        returnA -< Just picked
      else returnA -< Nothing
  -- An accumulator that retains the last 'Just' value.
  mWord <- accum' Nothing mplus -< mPicked
  returnA -< fromJust mWord

attempts :: Int
attempts = 5

livesLeft :: Int -> String
livesLeft hung = "Lives: ["
              ++ replicate (attempts - hung) '#'
              ++ replicate hung ' '
              ++ "]"

hangman :: StdGen -> Circuit String (Bool, [String])
hangman rng = proc userInput -> do
    word <- getWord rng -< ()
    let letter = listToMaybe userInput
    guessed <- updateGuess -< (word, letter)
    hung <- updateHung -< (word, letter)
    end <- delayedEcho True -< not (word == guessed || hung >= attempts)
    let result = if word == guessed
                   then [guessed, "You won!"]
                   else if hung >= attempts
                       then [guessed, livesLeft hung, "You died!"]
                       else [guessed, livesLeft hung]
    returnA -< (end, result)
  where
    updateGuess :: Circuit (String, Maybe Char) String
    updateGuess = accum' (repeat '_') $ \(word, letter) guess ->
        case letter of
            Just l  -> map (\(w, g) -> if w == l then w else g) (zip word guess)
            Nothing -> take (length word) guess

    updateHung :: Circuit (String, Maybe Char) Int
    updateHung = proc (word, letter) -> do
        total -< case letter of
            Just l  -> if l `elem` word then 0 else 1
            Nothing -> 0

runHangman :: IO ()
runHangman = do
    rng <- getStdGen
    interact $ unlines                      -- Concatenate lines out output
        . ("Welcome to Arrow Hangman":)     -- Prepend a greeting to the output
        . concat . map snd . takeWhile fst  -- Take the [String]s as long as the first element of the tuples is True
        . runCircuit (hangman rng)          -- Process the input lazily
        . ("":)                             -- Act as if the user pressed ENTER once at the start
        . lines                             -- Split input into lines