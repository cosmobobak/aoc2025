module Main where

import Data.List.Split
import Debug.Trace
import Text.Printf
import Data.List.Duplicate

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
invalid :: [Int] -> Int -> [Int]
invalid [a, b] n = concat cnts
  where
    da = digits a
    db = digits b
    -- digits / blocks
    hda = da `div` n
    hdb = db `div` n
    -- all the exponents for each block size
    exps = [e `div` n | e <- [da..db], e `mod` n == 0]
    -- if `a` is 13, da is 2,
    -- and we should start at 1
    -- if `a` is 127125, da is 6,
    -- and we should start at 125.
    -- s_lo = a `div` (10 ^ hda)
    -- s_hi = a `mod` (10 ^ hda)
    s_bits = [(a `div` 10 ^ (hda * i)) `mod` 10 ^ hda | i <- [0..(n-1)]]
    -- s = if even da then max s_lo s_hi else 10 ^ hda
    s = if da `mod` n == 0 then min (last s_bits + 1) (maximum s_bits) else 10 ^ hda
    -- converse for b
    -- e_lo = b `div` (10 ^ hdb)
    -- e_hi = b `mod` (10 ^ hdb)
    e_bits = [(b `div` 10 ^ (hdb * i)) `mod` 10 ^ hdb | i <- [0..(n-1)]]
    -- e = if even db then min e_lo e_hi else 10 ^ hdb - 1
    e = if db `mod` n == 0 then max (minimum e_bits) (last e_bits - 1) else 10 ^ hdb - 1

    cnts = [process s e i (length exps) exp n | (i, exp) <- zip [0..] exps]

-- for each range in exps
-- if the range is the first one, start at s
-- if the range is the final one, end at e
-- otherwise, start at 1 and end at 10^digits - 1
process :: Int -> Int -> Int -> Int -> Int -> Int -> [Int]
process start end index l exponent n = map expify [lo..hi]
  where
    lo = if index == 0 then start else 0
    hi = if index + 1 == l then end else 10 ^ exponent - 1
    -- expify x = x + x * 10 ^ exponent
    expify x = sum [x * 10 ^ (exponent * i) | i <- [0..(n-1)]]

solve1 :: [[Int]] -> Int
solve1 x = sum values
  where
    streams = map (`invalid` 2) x
    values = deleteDups $ concat streams

solve2 :: [[Int]] -> Int
solve2 x = sum values
  where
    streams = concat [map (`invalid` blocks) x | blocks <- [2..200]]
    values = deleteDups $ concat streams

operate :: String -> String
operate input = show $ solve2 ranges
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

