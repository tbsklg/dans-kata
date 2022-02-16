module Main where

import Data.Maybe (fromJust)
import Lib (fixed, random)
import System.Environment (getArgs)
import System.IO (IOMode (ReadMode), hGetContents, openFile)

dispatch :: [(String, IO ())]
dispatch =
  [ ("fixed", fixed'),
    ("random", random')
  ]

fixed' :: IO ()
fixed' = do
  file <- openFile "app/resources/input.txt" ReadMode
  contents <- hGetContents file
  putStrLn $ unlines . fixed . lines $ contents

random' :: IO ()
random' = do
  file <- openFile "app/resources/input.txt" ReadMode
  contents <- hGetContents file
  putStrLn $ unlines . random . lines $ contents

main :: IO ()
main = do
  c <- getArgs

  if null c
    then fromJust . lookup "fixed" $ dispatch
    else fromJust . lookup (head c) $ dispatch
