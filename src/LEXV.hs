module LEXV where

-- | Title: Ordering Strings of Varying Length Lexicographically
-- | Desc:  Get all the possible combinations the given alphabet, ordered
-- | Link:  http://rosalind.info/problems/lexv/

-- | Input
main :: IO ()
main = do
  let alphabet = filter (/=' ') "O Z Q L P M H X I E B"
      bound    = 4 :: Int
  writeFile "IO Files/Output.txt" $ unlines $ allWords bound alphabet

-- | My original solution
allWords :: Int -> String -> [String]
allWords 0 _ = []
allWords m xs =
  concat [ map (x:) y | x <- xs, let y = "" : allWords (m - 1) xs ]
