module IEV where

-- | Title: Calculating Expected Offspring
-- | Desc:  Return expected number of offspring with the dominant phenotype
-- | Link:  http://rosalind.info/problems/iev/

-- | Input
main :: IO ()
main = do
  let couples = [16616, 19545, 16715, 18633, 16151, 18455 :: Double]
  print $ offspring couples

-- | A slightly refactored solution
probabilities :: [Double]
probabilities = [1.0, 1.0, 1.0, 0.75, 0.5, 0.0]

offspring :: [Double] -> Double
offspring cs = 2 * sum (zipWith (*) cs probabilities)
