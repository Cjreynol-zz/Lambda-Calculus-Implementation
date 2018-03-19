{-|
Module      : Term
Description : Untyped lambda terms using De Bruijn indices to avoid variable 
                capture
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}
module Term(
    Term(..),
    ReductionSequence,
    termListToStr
    ) where


import Nat (Nat)


-- | Untyped lambda terms using De Bruijn indices to avoid variable capture
data Term = Var Nat | 
            Lam Term | 
            App Term Term
    deriving (Eq)

instance Show Term where
    show (Var n) = show n
    show (Lam t) = "(\\." ++ (show t) ++ ")"
    show (App t t') = "(" ++ (show t) ++ " " ++ (show t') ++ ")"

-- | Provides extra context to specific functions dealing with term lists
type ReductionSequence = [Term]

-- | Pretty prints a reduction sequence of terms
termListToStr :: ReductionSequence -> String
termListToStr [] = ""
termListToStr (x:[]) = (show x)
termListToStr (x:xs) = (show x) ++ " ~>\n" ++ (termListToStr xs)

