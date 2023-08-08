module Main (main) where

import Lib (findsols')
import Hutton (solutions'')

main :: IO ()
main = do 
    let mysols = findsols' [1,3,7,10,25,30] 765
    putStrLn "My solution"
    putStrLn . show . head $ mysols
    putStrLn . ("Total number of solutions:"++) .show . length $ mysols
    putStrLn "Professor Hutton Solution''"
    let hsols = solutions'' [1,3,7,10,25,30] 765
    putStrLn . show . head $ hsols
    putStrLn . ("Total number of solutions:"++) .show . length $ hsols
