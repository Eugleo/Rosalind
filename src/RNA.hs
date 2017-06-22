module RNA where

-- | Title: Transcribing DNA into RNA
-- | Desc:  Transcribe the DNA string to RNA (T -> U)
-- | Link:  http://rosalind.info/problems/rna/

-- | Input
main :: IO ()
main = do
  dna <- readFile "IO Files/RNAinput.txt"
  writeFile "IO Files/Output.txt" $ transcribe dna

-- | My original solution
transcribe :: String -> String
transcribe = map (\c -> if c == 'T' then 'U' else c)
