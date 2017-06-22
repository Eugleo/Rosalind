module LEXF where

import Control.Monad (replicateM)

-- | Title: Enumerating k-mers Lexicographically
-- | Desc:  Get all the possible combinations of n elements, ordered
-- | Link:  http://rosalind.info/problems/lexf/

-- | Input
main :: IO ()
main = do
  let alphabet = filter (/=' ') "A B C D E F G H I J"
      choose   = 2 :: Int
  writeFile "IO Files/Output.txt" $ unlines $ kmers choose alphabet

-- | My original solution
kmers :: Int -> String -> [String]
kmers 0 _  = [[]]
kmers n xs = [ x : y | x <- xs, y <- kmers (n - 1) xs ]

-- | A cool solution from Rosalind
kmers' :: Monad m => Int -> m a -> m [a]
kmers' = replicateM
