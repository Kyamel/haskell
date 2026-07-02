
module Aula5 where 
  --  Tipos de dados recursivos

  data TSil a = Emp
    | Snoc (TSil a) a deriving (Show,Eq)

  len :: TSil a -> Int
  len Emp = 0
  len (Snoc xs _) = len xs + 1

  lst2TSil :: [a] -> TSil a
  lst2TSil xs = l2t (reverse xs)
    where 
      l2t [] = Emp
      l2t (x:xs) = Snoc (lst2TSil xs) x

  lst2TSil2 :: [a] -> TSil a
  lst2TSil2 [] = Emp
  lst2TSil2 (x:xs) = fr x (lst2TSil2  xs)
    where
      fr x Emp = Snoc Emp x
      fr x (Snoc zs y) = Snoc (fr x zs) y

  data BinTree a = TNil 
                  | Bin (BinTree a) a (BinTree a)
                  deriving (Show, Eq)

  height :: BinTree a -> Int
  height TNil = 0
  height (Bin te r td) = (max (height te) (height td)) + 1

  -- is binary search tree
  isbst :: Ord a => BinTree a -> Bool
  isbst TNil = True 
  isbst (Bin TNil r TNil) =  True
  isbst (Bin  te@(Bin _ re _) r TNil) = (re < r) && (isbst te)
  isbst (Bin TNil r td@(Bin _ rd _)) = (r >= rd) && (isbst td)
  isbst (Bin te@(Bin _ re _) r td@(Bin _ rd _))
    = (re < r) && (rd >= r) && (isbst te) && (isbst td)
    
  bsearch :: Ord a => BinTree a -> a -> Bool
  bsearch TNil k = False
  bsearch (Bin te r td) k
    | k < r = bsearch te k
    | k > r =  bsearch td k
    |otherwise = r == k

  mapT :: (a -> b) -> BinTree a -> BinTree b
  mapT f TNil = TNil
  mapT f (Bin te r td) = Bin (mapT f te) (f r) (mapT f td)

  foldT :: (a -> b -> b -> b) -> b -> BinTree a -> b
  foldT f v TNil = v
  foldT f v (Bin te r td) = f r (foldT f v te) (foldT f v td)

  hft :: BinTree a -> Int
  hft t = foldT (\r re rd -> (max re rd) + 1) 0 t

  fbs :: Ord a => BinTree a -> a -> Bool
  fbs t k = foldT (\r re rd -> re || rd || r == k) False  t

  data RoseTree a = Rose a [RoseTree a]
    deriving Show


  -- map e fold para RoseTree prova
  
  data Exp = Add Exp Exp
    | Sub Exp Exp
    | Mult Exp Exp
    | Div Exp Exp
    | Lit Int 
    | Var String
    deriving Show

  eval :: [(String, Int)] -> Exp -> Maybe Int
  eval e (Lit n) = Just n
  eval e (Var s) = lookup s e
  eval e (Add ee ed) = zipMaybe (+) (eval e ee) (eval e ed)
  eval e (Sub ee ed) = zipMaybe (-) (eval e ee) (eval e ed)
  eval e (Mult ee ed) = zipMaybe (*) (eval e ee) (eval e ed)
  eval e (Div ee ed) = zipMaybe (div) (eval e ee) (eval e ed)

  zipMaybe :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
  zipMaybe f (Just x) (Just y) = Just (f x y)
  zipMaybe _ _ _ = Nothing



