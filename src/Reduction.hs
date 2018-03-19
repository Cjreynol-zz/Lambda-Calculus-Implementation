{-|
Module      : Reduction
Description : Functions for beta reduction and full normalization of terms
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}
module Reduction(
    betaReduce,
    normalize,
    getReductionSeq
    ) where


import Term         (Term(..), ReductionSequence)
import Nat          (Nat(Zero))
import Substitution (subst)


-- | Reduces a term one step using leftmost reduction to guarantee reaching a 
-- normal form if it exists.
betaReduce :: Term -> Term
betaReduce (Var n) = Var n
betaReduce (Lam t) = Lam (betaReduce t)
betaReduce (App (Lam t) t') = subst t Zero t'
betaReduce (App t t') 
    | redexExists t = App (betaReduce t) t'
    | otherwise = App t (betaReduce t')

-- | Beta reduces until a normal form is reached.
normalize :: Term -> Term
normalize t 
    | redexExists t = normalize (betaReduce t)
    | otherwise = t

redexExists :: Term -> Bool
redexExists (Var _) = False
redexExists (Lam t) = redexExists t
redexExists (App (Lam _) _) = True
redexExists (App t t') = (redexExists t) || (redexExists t')

-- | Normalizes and accumalates each term into a list so the full sequence 
-- can be processed.  Used for command-line output.
getReductionSeq :: Term -> ReductionSequence
getReductionSeq t 
    | redexExists t = t : (getReductionSeq $ betaReduce t)
    | otherwise = [t]

