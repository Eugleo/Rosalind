module PROT where

import Rosalind
import Data.Maybe (fromJust)

-- | Title: Translating RNA into Protein
-- | Desc:  Return the prtein string encoded by input
-- | Link:  http://rosalind.info/problems/prot/

type Codons = [(String, String)]

-- | Input
main :: IO ()
main = do
  input      <- readFile "IO Files/PROTinput.txt"
  codonTable <- readFile "IO Files/RNACodonTable.txt"
  let codonMap = codonMapFromString codonTable
  writeFile "IO Files/Output.txt" $ fromJust $ translate codonMap input


-- | My originial solution
translate :: Codons -> String -> Maybe String
translate cds (x:y:z:xs) = do
  p  <- lookup [x, y, z] cds
  ps <- translate cds xs
  return (p ++ ps)
translate _   _          = Just []
