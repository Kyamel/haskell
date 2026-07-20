module Q4 where

    data LE = And LE LE
        | Or LE LE
        | Not LE
        | Const Bool
        | Var String
        deriving Show

    eval :: [(String, Bool)] -> LE -> Maybe Bool
    eval ambiente (Const b) = Just b

    eval ambiente (Var nome) =
    lookup nome ambiente

    eval ambiente (Not expressao) = do
    valor <- eval ambiente expressao
    return (not valor)

    eval ambiente (And esquerda direita) = do
    valorEsquerda <- eval ambiente esquerda
    valorDireita  <- eval ambiente direita
    return (valorEsquerda && valorDireita)

    eval ambiente (Or esquerda direita) = do
    valorEsquerda <- eval ambiente esquerda
    valorDireita  <- eval ambiente direita
    return (valorEsquerda || valorDireita)