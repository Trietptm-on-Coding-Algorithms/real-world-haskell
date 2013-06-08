data Tree a = Node a (Tree a) (Tree a)
            | Empty
            deriving Show

height :: Tree a -> Int
height (Node _ x y) = 1 + max (height x) (height y)
height Empty = 0
