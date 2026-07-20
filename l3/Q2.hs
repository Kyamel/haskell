module Q2 where

    instance (Eq a, Eq b) => Eq (Tree a b) where
        Leaf x == Leaf y
        = x == y
        Node v1 e2 d3 == Node v2 e2 d2
        = v1 == v1
        && e1 == e2
        && d1 == d2

    _ == _ = False
