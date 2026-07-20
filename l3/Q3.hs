module Q3 where

  data RoseTree a = Branch a [ RoseTree a ]
  deriving (Show, Eq)

  mapRT :: (a -> b) -> RoseTree a -> RoseTree b
  mapRT f (Branch a rs)
    = Branch (f a) (map (mapRT f) rs)

  foldrRT :: (a -> [b] -> b) -> b -> RoseTree a -> b
  foldrRT f b (Branch a rs) = f a (map (foldrRT f b) rs)

  mapRTFromFold :: (a -> b) -> RoseTree a -> RoseTree b
  mapRTFromFold f rt = foldrRT passo _ rt
    where passo x acc
      = Branch (f x) acc

  altura :: RoseTree a -> Int
  altura rt = foldrRT passo 0 rt
    where
      passo _ acc = 1 + maximum acc
      passo _ [] = 1
