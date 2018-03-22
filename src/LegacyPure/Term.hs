{-|
Module      : LegacyPure.Term
Description : Untyped lambda terms using De Bruijn indices to avoid variable 
                capture
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}
module LegacyPure.Term(
    Term(..),
    betaReduce,
    normalize
    ) where


import              Nat                 (Nat(..), natAdd, natPred)


-- | Untyped lambda terms using De Bruijn indices to avoid variable capture
data Term = Var Nat | 
            Lam Term | 
            App Term Term

instance Show Term where
    show (Var n) = show n
    show (Lam t) = "(\\." ++ (show t) ++ ")"
    show (App t t') = "(" ++ (show t) ++ " " ++ (show t') ++ ")"

instance Eq Term where
    (==) t1 t2 = treeEq (normalize t1) (normalize t2)

treeEq :: Term -> Term -> Bool
treeEq (Var n) (Var n') = if n == n' then True else False
treeEq (Lam t) (Lam t') = treeEq t t'
treeEq (App t1 t2) (App t1' t2') = treeEq t1 t1' && (treeEq t2 t2')
treeEq _ _ = False

-- | Reduces a term one step using leftmost reduction to guarantee reaching a 
-- normal form if it exists.
betaReduce :: Term -> Term
betaReduce (Var n) = Var n
betaReduce (Lam t) = Lam (betaReduce t)
betaReduce (App (Lam t) t') = subst t Zero t'
betaReduce (App t t') 
    | redexExists t = App (betaReduce t) t'
    | otherwise = App t (betaReduce t')

redexExists :: Term -> Bool
redexExists (Var _) = False
redexExists (Lam t) = redexExists t
redexExists (App (Lam _) _) = True
redexExists (App t t') = (redexExists t) || (redexExists t')

-- | Substitutes a Term into a another Term, keeping indices referencing the 
-- proper binder.  
-- 
-- The binder a term is associated with could be implicit if the variable is 
-- free.
subst :: Term -> Nat -> Term -> Term
subst (Var n) m t'
    | n < m = Var n
    | n > m = Var (natPred n)
    | otherwise = rename n Zero t'
subst (App t1 t2) m t' = App (subst t1 m t') (subst t2 m t')
subst (Lam t) m t' = Lam (subst t (Succ m) t')

rename :: Nat -> Nat -> Term -> Term
rename m i (Var j)
    | j < i = Var j
    | otherwise = Var (natPred (natAdd j m))
rename m i (App t1 t2) = App (rename m i t1) (rename m i t2)
rename m i (Lam t) = Lam (rename m (Succ i) t)

-- | Beta reduces until a normal form is reached.
normalize :: Term -> Term
normalize t
    | redexExists t = normalize $ betaReduce t
    | otherwise = t
