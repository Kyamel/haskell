module Q4 where

  data LE = And LE LE
    | Or LE LE
    | Not LE
    | Const Bool
    | Var String
    deriving Show

  eval :: [(String,Bool)] -> LE -> Maybe Bool
  eval [(x:xs, b)] Or = eval [(xs,b)] le || eval((xs,b) le)

