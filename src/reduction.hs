module Reduction where

import Term
import Nat
import Subst


betaReduce :: Term -> Term
betaReduce (Var n) = Var n
betaReduce (Lam t) = Lam (betaReduce t)
betaReduce (App (Lam t) t') = subst t Zero t'
betaReduce (App t t') 
                    | redexExists t = App (betaReduce t) t'
                    | otherwise = App t (betaReduce t')

normalize :: Term -> Term
normalize t 
        | redexExists t = normalize (betaReduce t)
        | otherwise = t

redexExists :: Term -> Bool
redexExists (Var n) = False
redexExists (Lam t) = redexExists t
redexExists (App (Lam t) t') = True
redexExists (App t t') = (redexExists t) || (redexExists t')

getReductionSeq :: Term -> [Term]
getReductionSeq t = reverse $ getReductionSeqHelper t []

getReductionSeqHelper :: Term -> [Term] -> [Term]
getReductionSeqHelper t l 
                    | redexExists t = getReductionSeqHelper (betaReduce t) (t:l)
                    | otherwise = (t:l)

