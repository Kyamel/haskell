module Aula3 where
  
  mcurry :: (a -> b -> c) -> (a,b) -> c
  mcurry f = \(x,y) -> f x y

  --muncurry :: ((a, b) -> c) -> a -> b -> c
  muncurry g = \x y -> g (x,y)

  fun x y = x < 2*y;

  quickSort :: (a -> a -> Bool) ->  [a] -> [a]
  quickSort c [] = []
  quickSort c [x] = [x]
  quickSort c (x:xs) = (quickSort c (mfilter (c x) xs)) 
                    ++ [x] 
                    ++ (quickSort c (mfilter (\y -> not (c x y)) xs))

  mfilter :: (a -> Bool) -> [a] -> [a]
  mfilter p [] = []
  mfilter p (x:xs)
    | p x = x:mfilter p xs
    | otherwise = mfilter p xs

  smallers x xs = mfilter(<x) xs
  greaters x xs = mfilter (>= x) xs

  takeWhl p [] = []
  takeWhl p (x:xs)
    | p x = x : takeWhl p xs
    | otherwise = []

  dropWhl p [] = []
  dropWhl p (x:xs)
    | p x = dropWhl p xs
    | otherwise = xs
  
  mmap :: (a -> b) -> [a] -> [b]
  mmap f [] = []
  mmap f (x:xs) = (f x):mmap f xs

  mzip :: [a] -> [b] -> [(a,b)]
  mzip [] _ = []
  mzip _ [] = []
  mzip (x:xs) (y:ys) = (x,y):mzip xs ys

  mzipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
  mzipWith _ [] _ = []
  mzipWith _ _ [] = []
  mzipWith f (x:xs) (y:ys) = (f x y):mzipWith f xs ys
  
  mfoldr :: (a -> b -> b ) -> b -> [a] -> b
  mfoldr f v [] = v
  mfoldr f v (x:xs) = f x (foldr f v xs)

  fmmap f xs = mfoldr g v xs
    where v = []
          g z zs = (f z):zs

  -- zipWith usando foldr eh possivel?
  fmfilter p xs = mfoldr g v xs
    where v = []
      g z zs = (f z):zs

  fmfilter p xs = mfoldr g v xs
    where v = []
      g z zs
        | p z = z:zs
        | otherwise = zs

  
  rev [] = []
  rev (x:xs) = (rev xs) ++ [x]
  
  revc :: [a] -> [a]
  revc xs = revc1 xs []
    where
      revc1 [] b = b
      revc1 (x:xs) b = revc1 xs (x:b)


  mfoldl :: (b -> a -> b) -> b -> [a] -> b
  mfoldl _ v [] = v
  mfoldl f v (x:xs) = mfoldl f (f v x) xs

  -- expresar foldl em termo de foldr
  
  flp :: (a -> b -> c) -> b -> a -> c
  flp f x y = f y x

  -- List comprehenshion
  
  -- [ pattern | x1 <- g1, x2 <- g2, precidate x1 x2 ]
  
  filerLc p xs = [y | y <- xs, p y]

  permutate :: [a] -> [[a]]
  permutate [] = [[]]
  permutate (x:xs) = concat [ insIn x ws | ws <- permutate xs]

  insIn x [] = [[x]]
  insIn x (z:zs) = (x:zs:zs) : (map (z:) (insIn x zs))

  -- lcMap f xs = [f x \ <- xs]
