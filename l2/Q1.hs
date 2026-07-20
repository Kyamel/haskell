module Q1 where
  swapfl :: [a] -> [a]
  swapfl [] = []
  swapfl [x] = [x]
  swapfl [x] = [x]
  swapfl (x:xs) = troca x xs
    where
      troca primeiro [ultimo] = [ultimo, primeiro]
      troca primeiro (y:ys) =
        case troca primeiro ys of
          (z:zs) -> z : y : zs
  

  maxList :: [Int] -> Int
  maxList [] = 0
  maxList [x] = x
  maxList (x:xs) = max x (maxList xs)


  minList :: [Int] -> Int
  minList [] = 0
  minList [x] = x
  minList (x:xs) = min x (minList xs)


  split3 :: [a] -> [[a]]
  split3 [] = []
  split3 (a:b:c:xs) = [a,b,c] : split3 xs
  split3 xs = [xs]


  splitOn :: Eq a => a -> [a] -> [[a]]
  splitOn _ [] = [[]]
  splitOn s (x:xs)
    | x == s = [] : splitOn s xs
    | otherwise = 
      case splitOn s xs of
        (y:ys) -> (x:y):ys 


  intersperse :: a -> [a] -> [a]
  intersperse _ [] = []
  intersperse _ [x] = [x]
  intersperse e (x:xs) = x: e: intersperse e xs

  merge :: [Int] -> [Int] -> [Int]
  merge [] a = a
  merge a [] = a
  merge (x:xs) (y:ys) = x: y: merge xs ys


  digits :: Int -> [Int]
  digits n
    | n < 10 = [n]
    | otherwise = digits (n `div` 10) ++ (n `mod` 10)


  digits2int :: [Int] -> Int
  digits2int xs = aux 0 xs
    where
      aux acumulador [] = acumulador
      aux acumulador (x:xs) =
        aux (acumulador * 10 + x) xs

  zip :: [a] -> [b] -> [(a,b)]
  zip _ [] = []
  zip [] _ = []
  zip (x:xs) (y:ys) = (x,y): zip xs ys
