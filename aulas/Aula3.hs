module Aula4 where
  
  data MBool = T | F deriving Show

  mand :: MBool -> MBool -> MBool
  mand T b = b
  mand F _ = F

  data LInt = Nil Int
    | Con Int LInt deriving Show
   
  data Pessoa = PFisica String Int
    | PJuridica String [Int] deriving Show

  data Nat = Z
    | S Nat deriving (Show, Eq, Ord)
 
  soma :: Nat -> Nat -> Nat 
  soma Z x = x
  soma (S x) y = S(soma x y) 

  data ML a = Nil | Cons a (ML a) deriving Show 

  mlHead :: ML a -> a
  mlHead (Cons x _) = x

  mlConcat :: ML a -> ML a -> ML a
  mlConcat (Nil) xs = xs
  mlConcat  (Cons y ys) xs = Cons y (mlConcat ys xs)

  hs2ML :: ML a -> [a]
  hs2ML Nil = []
  hs2ML (Cons x xs) = x:hs2ML xs

  lookp :: Eq a => [(a,b)] -> a -> b
  lookp [] x = error (" No key " ++ (show x) ++ "in table ")
  loolp ((k,v):xs) x
    | k == x = v
    | otherwise = lookp xs x

  --data Maybe a = Just a| Nothing
  lookpc :: Eq a => [(a,b)] -> a -> Maybe b
  lookpc [] x = error (" No key " ++ (show x) ++ "in table ")
  lookpc ((k,v):xs) x
    | k == x = Just v
    | otherwise = lookp xs x

