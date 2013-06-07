lastButOne :: [a] -> a
lastButOne [] = error "lastButOne on empty list"
lastButOne [_] = error "lastButOne on singleton list"
lastButOne [x, _] = x
lastButOne (_:xs) = lastButOne xs
