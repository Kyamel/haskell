module Q1 where
 
  triple :: Int -> Int
  triple n = n * 3
  
  maxEven n
    | n `mod` 2 == 0 = n
    | otherwise = n + 1
  
  isOdd n = n `mod` 2  /= 0

  prod :: Int -> Int -> Int
  prod n1 n2 = n1 * n2
  
  squared :: Int -> Int
  squared n = n^2
