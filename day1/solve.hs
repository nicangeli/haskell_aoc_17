{-# LANGUAGE NegativeLiterals #-}

module Main(main) where

import Data.Char

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ part1 input
  print $ part2 input

part1 :: String -> Int
part1 = sum_valid_pairs . build_pairs

sum_valid_pairs :: [(Char, Char)] -> Int
sum_valid_pairs xs = sum $ map (digitToInt . fst) $ filter is_valid_pair xs

build_pairs :: String -> [(Char, Char)]
build_pairs digits = zip digits (tail digits ++ [head digits])

is_valid_pair :: (Char, Char) -> Bool
is_valid_pair (x,y)
  | x == y = True
  | otherwise = False

part2 :: String -> Int
part2 = sum_valid_pairs . build_pivot_pairs

build_pivot_pairs :: String -> [(Char, Char)]
build_pivot_pairs digits = zip digits (take (length digits) $ drop (length digits `div` 2) $ cycle digits)
