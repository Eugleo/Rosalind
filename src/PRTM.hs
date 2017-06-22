module PRTM where

import Rosalind
import Data.Maybe (fromJust)

-- | Title: Calculating Protein Mass
-- | Desc:  Calculate the mass of a protein string
-- | Link:  http://rosalind.info/problems/prtm/

-- | Input
main :: IO ()
main = do
  input      <- readFile "IO Files/PRTMinput.txt"
  tableInput <- readFile "IO Files/AAWeightTable.txt"
  print $ proteinWeight (proteinMassMapFromString tableInput) (init input)

-- | My original solution
proteinWeight :: [(Char, Double)] -> String -> Double
proteinWeight as = foldr (\c acc -> acc + fromJust (lookup c as)) 0
