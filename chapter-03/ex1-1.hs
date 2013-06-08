data List a = Cons a (List a)
            | Nil
            deriving (Show, Eq)

fromList (x:xs) = Cons x (fromList xs)
fromList [] = Nil

toList (Cons x y) = x : toList y
toList Nil = []
