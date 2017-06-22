module FIB where

-- | Title: Rabbits and Recurrence Relations
-- | Desc:  Count the number of rabbits after n months if they have k kids
-- | Link:  http://rosalind.info/problems/fib/

-- | Input
main :: IO ()
main = do
  let (n, k) = (36, 3)
  print $ fibonacci n k

-- | A refactored solution
fibonacci :: Int -> Int -> Int
fibonacci n k = snd $ iterate (\ (a, b) -> (b, k*a + b)) (1,1) !! (n-2)

-- | My original solution
fibonacci' :: Int -> Int -> Int
fibonacci' n k = go n (1, 1)
 where
  go 2 (_, b) = b
  go m (a, b) = go (m - 1) (b, k * a + b)


