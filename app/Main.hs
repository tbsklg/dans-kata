module Main where

import Lib (solve, solvePartTwo)
import System.IO (IOMode (ReadMode), hGetContents, openFile)

main :: IO ()
main = do
  handle <- openFile "app/resources/input.txt" ReadMode
  contents <- hGetContents handle
  putStrLn $ unlines . solvePartTwo . lines $ contents
