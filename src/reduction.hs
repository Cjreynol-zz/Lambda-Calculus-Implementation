module Reduction where

import Term
import Nat
import Subst


betaReduce :: Term -> Term
betaReduce (Var n) = Var n
betaReduce (Lam t) = Lam (betaReduce t)
betaReduce (App (Lam t) t') = subst t Zero t'
betaReduce (App t t') = if redexExists t 
                            then App (betaReduce t) t' 
                            else App t (betaReduce t')


normalize :: Term -> Term
normalize t = if redexExists t 
                then normalize (betaReduce t) 
                else t

redexExists :: Term -> Bool
redexExists (Var n) = False
redexExists (Lam t) = redexExists t
redexExists (App (Lam t) t') = True
redexExists (App t t') = (redexExists t) || (redexExists t')
