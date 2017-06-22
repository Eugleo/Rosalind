module GC where

import Rosalind
import Data.Function (on)
import Data.List (genericLength, maximumBy)

-- | Title: Computing GC Content
-- | Desc:  Return the ID of a string with the highest GC content
-- | Link:  http://rosalind.info/problems/gc/

type Fasta = [(String, String)]

-- | Input
main :: IO ()
main = do
  input <- readFile "IO Files/GCinput.txt"
  let dna = fastaFromString input
  print $ highestGC dna

-- | My original solution
highestGC :: Fasta -> (String, Double)
highestGC x = maximumBy (compare `on` snd) $ helper <$> x
 where
  helper (a, b) = (a, 100 * content b / genericLength b)
  content = foldl (\a c -> if c `elem` "GC" then a + 1 else a) 0
