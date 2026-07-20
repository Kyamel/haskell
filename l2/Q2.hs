module Q2 where 

  (+c+) : : [a] −> [a] −> [a]
  (+c+) [] ys = ys
  (+c+) (x:xs) ys = x:(xs++ys)

  -- Mostre que, para quaisquer listas xs, ys e zs, vale:
  -- xs++(ys++zs) == (xs ++ys)++zs
  until :: (a -> Bool) -> (a -> a) -> a -> a
  until cond passo valor
    | cond valor = valor
    | otherwise = until cond passo (passo valor)

  untilFinal :: (a -> Bool) -> (a -> a) -> a -> a
  untilFinal cond passo valor =
    let novoValor = passo valor
    in if condicao novoValor
        then novoValor
        else untilFinal cond passo novoValor

  zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
  zipWith _ _ [] = []
  zipWith _ [] _ = []
  zipWith fn (x:xs) (y:ys) = 
    fn x y : zipWith fn xs ys

  zipFromZipWith :: [a] -> [b] -> [(a,b)]
  zipFromZipWith xs ys =
    zipWith fn xs ys
    where
      fn a b = (a,b)

  -- use foldr:
  
  mmap :: (a -> b) -> [a] -> [b]
  mmap f xs = foldr passo [] xs
    where
      passo x acc = f x: acc
    
  mfilter :: (a -> Bool) -> [a] -> [a]
  mfilter f xs = foldr passo [] xs
    where 
      passo x acc
        | f x = x:acc
        | otherwise = acc
  
  mtakeWhile :: (a -> Bool) -> [a] -> [a]
  mtakeWhile f xs = foldr passo [] xs
    where
      passo x acc
        | f x = x:acc
        | otherwise = []

  mdropWhile :: (a -> Bool) -> [a] -> [a]
  mdropWhile _ [] = []
  mdropWhile f (x:xs)
    | f x = mdropWhile f xs
    | otherwise = x:xs  
  
  mmaximum :: Ord a => [a] -> a
  mmaximum [] = error "lista vazia"
  mmaximum xs = foldr passo x xs
    where 
      passo atual maior
        | atual > maior = atual
        | otherwise = maior
  
  mreverse :: [a] -> [a]
  mreverse xs = foldr passo [] xs
    where passo x acc =
      acc++[x]
  
  mspliton ::Eq a => a -> [a] -> [[a]]
  mspliton s xs = foldr passo [[]] xs
    where 
      passo x (z:zs)
        | x == s = []
        | otherwise = (x: z): zs
  
  mdigits2int :: [Int] -> Int
  mdigits2int xs  = foldl passo 0 xs
    where 
      passo x acc = 10*acc+x

  mfoldl :: (b -> a -> b) -> (b) -> [a] -> b
  mfoldl f inicial xs =
    foldr passo id xs inicial
      passo x continuar acc =
        continuar (f acc x)
