module Eval where

import Term
import PosInt
import Subst

-- evaluation (as shown in Chris Hankin's 
-- Introduction to Lambda Calculus for Computer Science)

eval :: Term -> Term
eval (Var n) = Var n
eval (Lam t) = Lam (eval t)
eval (App (Lam t) t') = subst t One t'
eval (App t t') = App (eval t) (eval t')

normalize :: Term -> Term
normalize t = if t == (eval t) then t else normalize (eval t)

