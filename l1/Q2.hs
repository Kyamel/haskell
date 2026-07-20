module Q2 where
  
  moveX :: Int -> (Int, Int) -> (Int, Int)
  moveX d (x, y) = (x+d, y)

  moveY :: Int -> (Int, Int) -> (Int, Int)
  moveY d (x, y) = (x, y+d)

  mirrorY :: (Int, Int) -> (Int, Int)
  mirrorY (x, y) = (x, -y)

  mirrorX :: (Int, Int) -> (Int, Int)
  mirrorX (x, y) = (-x, y)

  higher :: (Int,Int) -> (Int,Int) -> Bool
  higher (_, y1) (_, y2) = y1 > y2

  distance :: (Int,Int) -> (Int,Int) -> Float
  distance (x1, y1) (x2, y2) = 
    sqrt (
      fromIntegral (
        (x2 - x1) ^ 2 + (y2 - y1) ^ 2
      )
    )
  colinear :: (Int,Int) -> (Int,Int) -> (Int,Int) -> Bool
  colinear (x1, y1) (x2, y2) (x3, y3) = 
    (x2 - x1) * (y3 - y1) == (y2 - y1) * (x3 - x1)



