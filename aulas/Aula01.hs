module Aula01 where
  
  x = 10
  y = (1, 'a', False)
  y2 = (2, 'b', True)
  z = ('a',False,1)
  zs = [1,2,3]

  double x = 2*x

  fat 0 = 1
  fat n = n * fat (n-1)

  first :: (Int, Int) -> Int
  first (a,b) = a
  second(a,b) = b

  --soma 0 b = b
  soma n b 
      |n <= 0 = b
      |True = 1 + (soma (n-1) b)

  swap (a, b) = (b, a)

  quadruple = double.double

  identity = (\x -> x)
