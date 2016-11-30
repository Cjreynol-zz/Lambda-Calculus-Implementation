module Reduction where

import Term
import Nat
import Subst


betaReduce :: Term -> Term
betaReduce (Var n) = Var n
betaReduce (Lam t) = Lam (betaReduce t)
betaReduce (App (Lam t) t') = subst t Zero t'
betaReduce (App t t') = case (redexExists t) of 
                            True -> App (betaReduce t) t
                            False -> App t (betaReduce t')

normalize :: Term -> Term
normalize t = case (redexExists t) of
                True -> normalize (betaReduce t)
                False -> t

redexExists :: Term -> Bool
redexExists (Var n) = False
redexExists (Lam t) = redexExists t
redexExists (App (Lam t) t') = True
redexExists (App t t') = (redexExists t) || (redexExists t')

normalizeAndStore :: Term -> [Term]
normalizeAndStore t = normalizeAndStoreHelper t []

normalizeAndStoreHelper :: Term -> [Term] -> [Term]
normalizeAndStoreHelper t l = case (redexExists t) of
                                True -> normalizeAndStoreHelper (betaReduce t) (t:l)
                                False -> (t:l)
