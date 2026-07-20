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

  instance (Eq a, Eq b) => Eq (Tree a b) where
    Leaf x == Leaf y
      = x == y
    Node v1 e2 d3 == Node v2 e2 d2 
      = v1 == v1
      && e1 == e2
      && d1 == d2

   _ == _ = False

  data RoseTree a = Branch a [ RoseTree a ] 
    deriving (Show, Eq)

  mapRT :: (a -> b) -> RoseTree a -> RoseTree b
  mapRT f (Branch a rs) 
    = Branch (f a) (map (mapRT f) rs)

  foldrRT :: (a -> [b] -> b) -> b -> RoseTree a -> b
  foldrRT f b (Branch a rs) = f a (map (foldrRT f b) rs)

  mapRTFromFold :: (a -> b) -> RoseTree a -> RoseTree b
  mapRTFromFold f rt = foldrRT passo _ rt
    where passo x acc
      = Branch (f x) acc

  altura :: RoseTree a -> Int
  altura rt = foldrRT passo 0 rt
    where 
      passo _ acc = 1 + maximum acc
      passo _ [] = 1

  data LE = And LE LE
    | Or LE LE
    | Not LE
    | Const Bool
    | Var String
    deriving Show

  eval :: [(String,Bool)] -> LE -> Maybe Bool
  eval [(x:xs, b)] Or = eval [(xs,b)] le || eval((xs,b) le)

