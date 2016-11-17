module Subst where

import Term
import PosInt


-- substitution (as shown in Chris Hankin's 
-- Introduction to Lambda Calculus for Computer Science)

subst :: Term -> PosInt -> Term -> Term
subst (Var n) m t'
            | n < m = Var n
            | n > m = Var (piPred n)
            | otherwise = rename n One t'
subst (App t1 t2) m t' = App (subst t1 m t') (subst t2 m t')
subst (Lam t) m t' = Lam (subst t (Succ m) t')

rename :: PosInt -> PosInt -> Term -> Term
rename m i (Var j)
            | j < i = Var j
            | otherwise = Var (piPred (piAdd j m))
rename m i (App t1 t2) = App (rename m i t1) (rename m i t2)
rename m i (Lam t) = Lam (rename m (Succ i) t)
