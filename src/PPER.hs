module PPER where

-- | Title: Partial Permutations
-- | Desc:  Return the number of partial permutations P(n,k) modulo 1 000 000
-- | Link:  http://rosalind.info/problems/pper/

-- | Input
main :: IO ()
main = do
  let n = 98
      k = 8
  print $ pperms n k

-- | My original solution
pperms :: Integer -> Integer -> Integer
pperms n k = foldr (\a acc -> (a * acc) `mod` 10 ^ 6) 1 [(n - k + 1) .. n]
