module REVC where

import Data.Maybe (mapMaybe)

-- | Title: Complementing a Strand of DNA
-- | Desc:  Complement the dna sequence (A -> T, C -> G, and vice versa)
-- | Link:  http://rosalind.info/problems/revc/

-- | Input
main :: IO ()
main = do
  dna <- readFile "IO Files/REVCinput.txt"
  writeFile "IO Files/Output.txt" $ complement dna

-- | My original solution
complement :: String -> String
complement s =
  map
      ( \c -> case c of
        'A' -> 'T'
        'T' -> 'A'
        'C' -> 'G'
        'G' -> 'C'
      )
    $ filter (`elem`"ACGT")
    $ reverse s

-- | A cool solution from Rosalind
complement' :: String -> String
complement' s = reverse $ mapMaybe (`lookup`replacement) s
 where
  replacement = zip ['A', 'C', 'T', 'G'] ['T', 'G', 'A', 'C']
