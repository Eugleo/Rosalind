module SUBS where

import Data.List (tails, isPrefixOf)

-- | Title: Finding a Motif in DNA
-- | Desc:  Return all the occurencies of one string in the other
-- | Link:  http://rosalind.info/problems/subs/

-- | Input
main :: IO ()
main = do
  input <- readFile "IO Files/SUBSinput.txt"
  let [ss, sb] = lines input
  print $ unwords $ map (show . (+1)) $ occurencies sb ss

-- | My original solution
occurencies :: String -> String -> [Int]
occurencies sb ss = go 0
 where
  (l, k) = (length sb, length ss)
  go i | i >= k                   = []
       | take l (drop i ss) == sb = i : go (i + 1)
       | otherwise                = go (i + 1)

-- | A cool solution using the function isPrefixOf
occurencies' :: String -> String -> [(Int, String)]
occurencies' sb ss = filter (isPrefixOf sb . snd) $ zip [1 ..] $ tails ss
