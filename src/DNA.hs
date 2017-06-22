module DNA where

import Data.List (group, sort)

-- | Title: Counting DNA Nucleotides
-- | Desc:  Count the number of ACGT nucleobases in DNA string
-- | Link:  http://rosalind.info/problems/dna/

-- | Input
main :: IO ()
main = do
  dna <- readFile "IO Files/DNAinput.txt"
  print $ parse dna

-- | My original solution
parse :: String -> [Int]
parse xs = count <$> "ACGT"
 where
  count c = length $ filter (==c) xs

-- | Cool solution based on group and sort
parse' :: String -> [Int]
parse' xs = map length $ (group . sort) $ filter (`elem` "ACGT") xs
