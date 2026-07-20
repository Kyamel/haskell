module Polimorphism where
  
  -- polimorfismo parametrico universal
  identity :: p -> p
  identity x = x

  swap :: (a,b) -> (b,a)
  swap (x,y) = (y,x)

  (<@>) :: (a -> b) -> (b -> c) -> (a -> c)
  (<@>) f g x = g (f x)

  -- Listas
  -- [] Lista vazia
  -- x:xs Cons (x cabeca) (xs a cauda)
  --
  f :: [int] -> [Char]
  --f xs@[] = []
  f [] = []
  f (_:xs) = 'a':f xs

  lhead :: [a] -> a
  lhead [] = error "Nao possivel"
  lhead (x:_) = x

  ltail :: [a] -> [a]
  ltail [] = []
  ltail (x:xs) = xs

  at :: Int -> [a] -> a
  at 0 (x:_) = x
  at n (_:xs) = at (n-1) xs

  ltake :: Int -> [a] -> [a]
  ltake 0 _ = []
  ltake _ [] = []
  ltake n (x:xs) = x:(ltake (n-1) xs)

  llast :: [a] -> a
  llast [x] = x
  llast (x:xs) = llast xs
  
