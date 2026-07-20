module VaiCair where

    mzipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
    mzipWith f xs ys =
        foldr passo (const []) xs ys
        where
            passo x continuar [] = []
            passo x continuar (y:ys) =
                f x y : continuar ys



    mfoldl :: (b -> a -> b) -> b -> [a] -> b
    mfoldl f inicial xs =
        foldr passo id xs inicial
        where
            passo x continuar acc =
                continuar (f acc x)


    --lcMap f xs = map f xs
    --lcMap f xs = [f x | x <- xs]