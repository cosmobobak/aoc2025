module Main where

import Data.List.Split
import Debug.Trace
import Text.Printf

-- example input:
-- 11-22,95-115,998-1012
-- we need to split on ',', then '-'

-- an invalid ID is expressible as
-- x + 10^n * x, where n = digits(x)
-- so, to count the number of these
-- from a to b, we compute digits(a)
-- and digits(b), then, for each even
-- value in [d(a), d(b)], we loop over
-- x from 0 to 10^(n-1), adding one each
-- time.
invalid :: [Int] -> Int
invalid [a, b] = sum $ map sum cnts
  where
    da = digits a
    db = digits b
    hda = da `div` 2
    hdb = db `div` 2
    exps = [e `div` 2 | e <- [da..db], even e]
    -- if `a` is 13, da is 2,
    -- and we should start at 1
    -- if `a` is 127125, da is 6,
    -- and we should start at 125.
    s_lo = a `div` (10 ^ hda)
    s_hi = a `mod` (10 ^ hda)
    -- s = if even da then max s_lo s_hi else 10 ^ hda
    s = if even da then min (s_lo + 1) (max s_lo s_hi) else 10 ^ hda
    -- converse for b
    e_lo = b `div` (10 ^ hdb)
    e_hi = b `mod` (10 ^ hdb)
    -- e = if even db then min e_lo e_hi else 10 ^ hdb - 1
    e = if even db then max (min e_lo e_hi) (e_lo - 1) else 10 ^ hdb - 1

    cnts = [process s e i (length exps) exp | (i, exp) <- zip [0..] exps]

-- for each range in exps
-- if the range is the first one, start at s
-- if the range is the final one, end at e
-- otherwise, start at 1 and end at 10^digits - 1
process :: Int -> Int -> Int -> Int -> Int -> [Int]
process start end index l exponent = map (\x -> x + x * 10 ^ exponent) [lo..hi]
  where
    lo = if index == 0 then start else 0
    hi = if index + 1 == l then end else 10 ^ exponent - 1

solve :: [[Int]] -> Int
solve = foldr ((+) . invalid) 0

operate :: String -> String
operate input = show $ solve ranges
  where
    ranges = map (map read) pairs
    pairs = map (splitOn "-") tokens
    tokens = splitOn "," input

main :: IO ()
main = interact operate












-- | Number of digits in a @number :: 'Int64'@ in base 10.
digits :: Int -> Int
digits n
  | n == minBound = 19 -- "negate minBound" is out of range of Int64
  | n < 0         = go (negate n)
  | otherwise     = go n
  where
    -- Maximum is 9223372036854775807 for positive and 9223372036854775808
    -- for negative integer.
    go m
      | m < 10                 = 1
      | m < 100                = 2
      | m < 1000               = 3
      | m < 10000              = 4
      | m >= 10000000000000000 = 16 + go (m `quot` 10000000000000000)
      | m >= 100000000         = 8  + go (m `quot` 100000000)
      | otherwise              = 4  + go (m `quot` 10000)
        -- m >= 10000

