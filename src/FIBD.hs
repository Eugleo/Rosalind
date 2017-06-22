module FIBD where

-- | Title: Mortal Fibonacci Rabbits
-- | Desc:  The total number of pairs of rabbits that will live after n month
--          if they die after m months
-- | Link:  http://rosalind.info/problems/fibd/

-- | Input
main :: IO ()
main = do
  let n = 99
      m = 18
  print $ fibd n m

-- | My original solution
fibd :: Integer -> Integer -> [Integer]
fibd n m = go (replicate (fromIntegral m) 0) 1
 where
  go rs x
    | x == n    = rs
    | x == 1    = go (1 : 1 : 1 : rs) (x + 1)
    | otherwise = go ((head rs + rs !! 1 - rs !! fromIntegral m) : rs) (x + 1)
