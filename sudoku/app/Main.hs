module Main (main) where

import Sudoku (solve4, diabolical, printGrid)

main :: IO ()
main = printGrid . head . solve4 $ diabolical
