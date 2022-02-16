module Lib where

import Data.Function (on)
import Data.List (minimumBy, sortBy)

data Tone = C | C' | D | D' | E | F | F' | G | G' | A | B | H deriving (Show, Eq, Ord, Enum)

type Tune = [Tone]

solve :: [[Char]] -> [[Char]]
solve =
  map tune''
    . order
    . extract

solvePartTwo :: [[Char]] -> [[Char]]
solvePartTwo =
  map tune''
    . order'
    . extract

extract :: [[Char]] -> [Tune]
extract = map tune

tune :: [Char] -> Tune
tune [] = []
tune [x] = [tone x]
tune (x : y : ys)
  | y == '#' = tone' [x, y] : tune ys
  | otherwise = tone x : tune (y : ys)

tune'' :: Tune -> [Char]
tune'' = concatMap tune'

tune' :: Tone -> [Char]
tune' C' = "C#"
tune' D' = "D#"
tune' F' = "F#"
tune' G' = "G#"
tune' x = show x

tone :: Char -> Tone
tone 'C' = C
tone 'D' = D
tone 'E' = E
tone 'F' = F
tone 'G' = G
tone 'A' = A
tone 'B' = B
tone 'H' = H
tone _ = error ""

tone' :: [Char] -> Tone
tone' "C#" = C'
tone' "D#" = D'
tone' "F#" = F'
tone' "G#" = G'
tone' _ = error ""

scale :: [Tone]
scale = [C, C', D, D', E, F, F', G, G', A, B, H]

dist :: Tone -> Tone -> Int
dist f t = minimum [dist' f t, dist' t f]
  where
    dist' f' t' = length . takeWhile (/= t') . dropWhile (/= f') . cycle $ scale

costs :: Tune -> Tune -> Int
costs x = sum . zipWith dist x

costs' :: [Tune] -> Int
costs' [] = 0
costs' [_] = 0
costs' (x : y : xs) = costs x y + costs' (y : xs)

order :: [Tune] -> [Tune]
order [] = []
order l = order' (tail l) [head l]
  where
    order' [] x = x
    order' xs x = order' next (x ++ [lowest'])
      where
        next = filter (/= lowest') xs
        lowest' = lowest (last x : xs)

order' :: [Tune] -> [Tune]
order' l =
  fst
    . minimumBy (compare `on` snd)
    . map (\z -> (z, costs' z))
    $ [order . take (length l) . drop x . cycle $ l | x <- [0 .. (length l - 1)]]

lowest :: [Tune] -> Tune
lowest [] = []
lowest (x : xs) =
  fst
    . minimumBy (compare `on` snd)
    . map (\z -> (z, costs x z))
    $ xs
