module MRNA where

import Rosalind
import Data.Monoid (Product(..), getProduct)

-- | Title: Inferring mRNA from Protein
-- | Desc:  How many mRNA could've been used to construct the protein
-- | Link:  http://rosalind.info/problems/mrna/

-- | Input
main :: IO ()
main = do
  input      <- readFile "IO Files/MRNAinput.txt"
  tableInput <- readFile "IO FIles/RNACodonTable.txt"
  let table = codonMapFromString tableInput
  print $ rnaFromProtein table (init input)

-- | My original solutions
numberOfRNA :: [(String, String)] -> String -> Integer
numberOfRNA tb s = foldr (\(_, y) b -> if y == s then b + 1 else b) 0 tb

rnaFromProtein :: [(String, String)] -> String -> Integer
rnaFromProtein tb p =
  (3 * getProduct (foldMap (Product . numberOfRNA tb . (:[])) p)) `mod` 10 ^ 6

rnaFromProtein' :: [(String, String)] -> String -> Integer
rnaFromProtein' tb = foldr (\a b -> (numberOfRNA tb [a] * b) `mod` 10 ^ 6) 3
