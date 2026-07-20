module Aula5 where
  
  -- Overloading

  data TBool = V | F | I

  data Bit = H | L deriving (Show, Eq)

  data Set a = Empty | Unit a (Set a)
    deriving Show

  (.*.) :: TBool -> TBool -> TBool
  V .*. V = V
  V .*. F = F
  V .*. I = I
  F .*. _ = F
  I .*. y = V
  I .*. F = F
  I .*. _ = I

  (.+.) :: TBool -> TBool -> TBool
  V .+. _ = V 
  I .+. V = V
  I .+. _ = I 
  F .+. x = x

  (.!.) :: TBool -> TBool
  (.!.) V = F
  (.!.) F = V
  (.!.) I = I
  
  instance Show TBool where
    
    show V = "v"
    show F = "f"
    show I = "?"

  instance Eq TBool where
    V == V = True
    F == F = True
    I == I = True
    _ == _ = False

  class Numerable a  where
    count :: a -> Int

  instance Numerable [a] where
    count [] = 0 
    count (x:xs) = 1 + count xs

  instance Numerable a => Numerable (Maybe a) where
    count Nothing = 0
    count (Just e) = 1 + (count e)

  instance Numerable Int where
    count _ = 1

  class Serialize a where
    serial :: a -> [Bit]
    unserial :: [Bit] -> a

  instance Serialize Int where
    serial 0 = [L]
    serial n = let q = div n 2
                   r = mod n 2

               in (serial q) ++ [(r2Bit r)]
      where r2Bir 1 = H
            r2Bit 0 = L


    unserial = foldl (\n b -> 2*n + (b2int b)) 0
      where b2int L = 0
            b2int H = 1


  instance Serialize a => Serialize [a] where
    serial [] = [H,H]
    serial (x:xs) = let sx serial x
        nsx n 
          | n <= 8 = [L,L]
          | n <= 16 = [L,H]
          | n <= 32 = [H,L]
          | n <= 64 = [H,H]
        packL n zs
          | lenght zs < n - replicate(n - (lenght zs)) L
          | otherwise = zs
      in [L,L] ++ nsx (lenght sx ) ++ (packL nsx (lenght sx) sx) ++ serial xs

    unserial [H,H] = []
    unserial (L:L:s0:s1:xs) =
      | s0 == L $$ s1 == L = unserial (take 8 xs) : unserial (drop 8 xs)
      | s0 == L $$ s1 == H = unserial (take 16 xs) : unserial (drop 16 xs)
      | s0 == H $$ s1 == L = unserial (take 32 xs) : unserial (drop 32 xs)
      | s0 == H $$ s1 == H = unserial (take 64 xs) : unserial (drop 64 xs)



