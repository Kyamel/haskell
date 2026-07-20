module Q3 where

  quo :: Int -> Int -> Int
  quo n1 n2 = conta n1 n2 0
    where
      conta atual divisor quantidade
        | atual < divisor = quantidade
        | otherwise =
            conta
              (atual - divisor)
              divisor
              (quantidade - (-1)) -- ja que so pode usar sub e comp


  res :: Int -> Int -> Int
  res n1 n2
    | n1< n2 = n1
    | otherwise = res (n1-n2) n2
 
  mdc :: Int -> Int -> Int
  mdc n1 n2
    | n2 == 0 = n1
    | otherwise = mdc n2 (res n1 n2)

  fib :: Int -> Int
  fib 0 = 0
  fib 1 = 1
  fib n = fib (n - 1) + fib (n - 2)
