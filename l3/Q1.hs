module Q3 where

  data Atom = Atom String
    deriving (Show , Eq)

  data Tree a b
    = Laaf b
    | Node a (Tree a b) (Tree a b)
    deriving (Show, Eq)

  data Result a
    = Sucess a
    | Error String
    deriving (Show, Eq)



