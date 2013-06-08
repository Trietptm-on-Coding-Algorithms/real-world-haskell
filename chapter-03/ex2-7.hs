join :: a -> [[a]] -> [a]
join sep = foldr (\x acc -> x ++ (sep : acc)) []
