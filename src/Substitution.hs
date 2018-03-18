{-|
Module      : Substitution
Description : Substitution implementation for De Bruijn indexed lambda terms
Copyright   : (c) Chad Reynolds, 2018
-}
module Substitution(
    subst
    ) where


import Term (Term(..))
import Nat  (Nat(..), natAdd, natPred)


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

