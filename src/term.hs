module Term where

import PosInt


data Term = Var PosInt | 
            Lam Term | 
            App Term Term
    deriving (Eq)

instance Show Term where
    show (Var n) = show n
    show (Lam t) = "( \\. " ++ (show t) ++ ")"
    show (App t t') = "(" ++ (show t) ++ (show t') ++ ")"


