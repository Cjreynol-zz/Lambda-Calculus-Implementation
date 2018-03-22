{-|
Module      : Pure.Term
Description : Pure lambda calculus terms, implemented using a locally nameless 
                representation
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}
module Pure.Term(
    Term,
    locallyClosedCheck
    ) where


import LambdaTerm   (LambdaTerm(..))
import Nat          (Nat(..))


-- | Untyped lambda terms.
type Term = LambdaTerm ()

isLocallyClosed :: Term -> Bool
isLocallyClosed t = isLocallyClosed' Zero t

isLocallyClosed' :: Nat -> Term -> Bool
isLocallyClosed' n (BVar n') = n > n'
isLocallyClosed' _ (FVar _) = True
isLocallyClosed' n (Lam _ t') = isLocallyClosed' (Succ n) t'
isLocallyClosed' n (App t' t'') = (isLocallyClosed' n t') && (isLocallyClosed' n t'')

-- | Error checking for terms to ensure they are locally closed.  Used for 
-- parsing terms.
locallyClosedCheck :: Term -> Either String Term
locallyClosedCheck t
    | isLocallyClosed t = Right t
    | otherwise = Left "Invalid term, not locally closed.  Check bound variable indices."

