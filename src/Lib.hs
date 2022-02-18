module Lib where

import Data.Function (on)
import Data.List (minimumBy, sortBy)

data Tone = C | C' | D | D' | E | F | F' | G | G' | A | B | H deriving (Show, Eq, Ord, Enum)

type Tune = [Tone]

fixed :: [[Char]] -> [[Char]]
fixed =
  map fromTune
    . order
    . extract

random :: [[Char]] -> [[Char]]
random =
  map fromTune
    . order'
    . extract

extract :: [[Char]] -> [Tune]
extract = map asTune

asTune :: [Char] -> Tune
asTune [] = []
asTune [x] = [asTone x]
asTune (x : y : ys)
  | y == '#' = asTone' [x, y] : asTune ys
  | otherwise = asTone x : asTune (y : ys)

fromTune :: Tune -> [Char]
fromTune = concatMap fromTone

fromTone :: Tone -> [Char]
fromTone C' = "C#"
fromTone D' = "D#"
fromTone F' = "F#"
fromTone G' = "G#"
fromTone x = show x

asTone :: Char -> Tone
asTone 'C' = C
asTone 'D' = D
asTone 'E' = E
asTone 'F' = F
asTone 'G' = G
asTone 'A' = A
asTone 'B' = B
asTone 'H' = H
asTone _ = error ""

asTone' :: [Char] -> Tone
asTone' "C#" = C'
asTone' "D#" = D'
asTone' "F#" = F'
asTone' "G#" = G'
asTone' _ = error ""

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
