import Data.List (sortBy)

sortLL :: Ord a => [[a]] -> [[a]]
sortLL = sortBy (\x y -> length x `compare` length y)
