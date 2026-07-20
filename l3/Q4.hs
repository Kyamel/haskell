module Q4 where

    data LE = And LE LE
        | Or LE LE
        | Not LE
        | Const Bool
        | Var String
        deriving Show

    eval :: [(String, Bool)] -> LE -> Maybe Bool
    eval _ (Const b) = Just b

    eval ambiente (Var nome) =
    lookup nome ambiente

    eval ambiente (And e1 e2) =
    zipMaybe (&&) (eval ambiente e1) (eval ambiente e2)

    eval ambiente (Or e1 e2) =
    zipMaybe (||) (eval ambiente e1) (eval ambiente e2)

    eval ambiente (Not e) =
    mapMaybe not (eval ambiente e)

    zipMaybe :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
    zipMaybe f (Just x) (Just y) = Just (f x y)
    zipMaybe _ _ _ = Nothing

    mapMaybe :: (a -> b) -> Maybe a -> Maybe b
    mapMaybe f (Just x) = Just (f x)
    mapMaybe _ Nothing  = Nothing


    vars :: LE -> [String]
    vars (Const _)   = []
    vars (Var nome)  = [nome]
    vars (Not e)     = vars e
    vars (And e1 e2) = vars e1 ++ vars e2
    vars (Or e1 e2)  = vars e1 ++ vars e2


    instance Eq LE where
        Const b1 == Const b2 =
            b1 == b2

        Var nome1 == Var nome2 =
            nome1 == nome2

        Not e1 == Not e2 =
            e1 == e2

        And e1 e2 == And e3 e4 =
            e1 == e3 && e2 == e4

        Or e1 e2 == Or e3 e4 =
            e1 == e3 && e2 == e4

        _ == _ =
            False

