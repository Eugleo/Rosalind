module IPRB where

-- | Title: Mendel's First Law
-- | Desc:  Given three groups of organisms, determite the probability two of
--          them will have offsprings showing the dominant phenotype
-- | Link:  http://rosalind.info/problems/iprb/

-- | Input
main :: IO ()
main = do
  let (x, y, z) = (17, 25, 19 :: Double)
  print $ domProbability x y z

-- | A refactored solution
domProbability :: Fractional a => a -> a -> a -> a
domProbability x y z =
  let t = x + y + z
  in  1 - (y * z + 0.25 * y * (y - 1) + z * (z - 1)) / (t * (t - 1))

-- | My original solution
domProbability' :: Fractional a => a -> a -> a -> a
domProbability' x y z = let in p100 + p75 * 0.75 + p50 * 0.5
 where
  p100 = x / t + (y + z) / t * x / (t - 1)
  p75  = y / t * (y - 1) / (t - 1)
  p50  = y / t * z / (t - 1) + z / t * y / (t - 1)
  t    = x + y + z
