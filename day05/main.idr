import Data.String
import Data.Maybe
import Data.List1
import Data.List
import Data.Fin
import System.File

parse_pair : String -> List Integer
parse_pair = (\line => mapMaybe parseInteger (forget $ split (== '-') line))

run_to_fixpoint : Eq a => a -> (a -> a) -> a
run_to_fixpoint x f = let x' = f x in
  if x == x' then x else run_to_fixpoint x' f

-- implementation of one pass of merging overlapping intervals.
-- assumes input is sorted by the start of each interval.
merge_once : List (List Integer) -> List (List Integer)
merge_once [] = []
merge_once [x] = [x]
merge_once (x :: y :: xs) =
  case (x, y) of
    ([x_low, x_high], [y_low, y_high]) =>
      if y_low <= x_high + 1 then
        -- overlapping or touching intervals, merge them
        -- e.g. for [1,5] and [5,10], we want [1,10]
        merge_once ([x_low, max x_high y_high] :: xs)
      else
        -- non-overlapping, keep both
        x :: merge_once (y :: xs)
    -- dead case, should not happen if input is well-formed
    _ => x :: merge_once (y :: xs)

-- notes:
-- 1. `trim` cleans up whitespace.
-- 2. `split (== '\n')` splits the input into lines.
--    interestingly, it returns a `List1`, which is known to be non-empty.
-- 3. `forget` converts `List1` to `List`.
-- 4. `findIndex (== "")` finds the index of the first empty line,
--    which we can use to split the input into the two parts.
--    `findIndex` returns a `Maybe (Fin (length xs))`, meaning that /if/
--    an index is found, it is proven to be in-bounds of xs. Sick!
-- 5. `maybe` is like Rust's map_or_else - if the Maybe is Nothing,
--    it returns the default value (0 here), otherwise it applies the function
--    (finToNat here) to extract the value.
-- 6. `finToNat` converts our dependently-typed Fin index to a regular Nat.
operate : String -> String
operate xs = let lines = forget $ (split (== '\n') . trim) xs in
  let split_point = maybe 0 finToNat $ findIndex (== "") lines in
  let first_part = Data.List.take split_point lines in
  let second_part = Data.List.drop (split_point + 1) lines in
  let pairs = map parse_pair first_part in
  let values = mapMaybe parseInteger second_part in
  let n_in_bounds = length $ filter (\v =>
        any (\p => case p of
            [low, high] => low <= v && v <= high
            _ => False) pairs) values in
  -- this is a possible naïve implementation of part 2:
  -- let total_cover = length $ nub $ concat $ map (\pair => case pair of
  --       [low, high] => [low .. high]
  --       _ => []) pairs in
  -- unfortunately, for very large ranges, it's totally ridiculous.
  -- instead, we can sort the ranges and merge them.
  let sorted_pairs = sort pairs in
  let merged = run_to_fixpoint sorted_pairs merge_once in
  let total_cover = sum $ map (\p => case p of
        [low, high] => (high - low + 1)
        _ => 0) merged in
  (show n_in_bounds) ++ "\n" ++ (show total_cover) ++ "\n"

-- reïmplementation of haskell's `interact`.
interact : (String -> String) -> IO ()
interact f = do
  Right contents <- fRead stdin
    | Left err => putStrLn ("Error: " ++ show err)
  putStr (f contents)

main : IO ()
main = interact operate