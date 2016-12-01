module Term where

import Nat


data Term = Var Nat | 
            Lam Term | 
            App Term Term
    deriving (Eq)

instance Show Term where
    show (Var n) = show n
    show (Lam t) = "(\\." ++ (show t) ++ ")"
    show (App t t') = "(" ++ (show t) ++ " " ++ (show t') ++ ")"

termListToStr :: [Term] -> String
termListToStr [] = "No terms."
termListToStr (x:[]) = (show x)
termListToStr (x:xs) = (show x) ++ " ~>\n" ++ (termListToStr xs)
