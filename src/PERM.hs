module PERM where

import Data.List (nub, delete, intersperse)

-- | Title: Enumerating Gene Orders
-- | Desc:  Return the permutations of [1..n]
-- | Link:  http://rosalind.info/problems/perm/

-- | Input
main :: IO ()
main = do
  let n  = 5
  let ps = perms n
  writeFile "IO Files/Output.txt" $ unlines
    $ show (length ps) : map (unwords . map show) ps

-- | My original solution
perms :: Int -> [[Int]]
perms n = helper [1 .. n]
 where
  helper [] = [[]]
  helper xs = nub [ x : ps | x <- xs, ps <- helper (delete x xs) ]
