module SIGN where

import Data.List (delete, permutations, nub)
import Data.Traversable (sequence)

-- | Title: Enumerating Oriented Gene Orderings
-- | Desc:  Return the signed permutations of [-n..n]
-- | Link:  http://rosalind.info/problems/sign/

-- | Input
main :: IO ()
main = do
  let n  = 5
  let ps = signed n
  writeFile "IO Files/Output.txt" $ unlines
    $ show (length ps) : map (unwords . map show) ps

-- | My original solution
signed :: Int -> [[Int]]
signed n = helper n $ [-n .. -1] ++ [1 .. n]
 where
  helper 0 _  = [[]]
  helper _ [] = [[]]
  helper k xs =
    nub [ x : ps | x <- xs, ps <- helper (k - 1) (delete (-x) (delete x xs)) ]

-- | An ultra-cool solution from Rosalind
signed' :: Int -> [[Int]]
signed' n = concatMap permutations . sequence $ map (\x -> [-x, x]) [1..n]

