module HAMM where

-- | Title: Counting Point Mutations
-- | Desc:  Count the number of differences between two DNA strings
-- | Link:  http://rosalind.info/problems/hamm/

-- | Input
main :: IO ()
main = do
  input <- readFile "IO Files/HAMMinput.txt"
  let [s1, s2] = lines input
  print $ diff s1 s2

-- | A refactored solution
diff :: String -> String -> Int
diff x y = length $ filter (uncurry (/=)) $ zip x y

-- | My original solution
diff' :: String -> String -> Int
diff' x y = foldr (\ (a1, a2) b -> if a1 == a2 then b else b + 1) 0 $ zip x y
