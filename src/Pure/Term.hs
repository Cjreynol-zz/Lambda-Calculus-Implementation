{-|
Module      : Pure.Term
Description : Pure lambda calculus terms, implemented using a locally nameless 
                representation
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}
module Pure.Term(
    Term(..),
    Atom,
    locallyClosedCheck
    ) where


import qualified    LambdaTerm as L     (LambdaTerm(..))
import              Nat                 (Nat(..))


type Atom = Nat

data Term =   BVar Nat |
                FVar Atom |
                Lam Term |
                App Term Term
                deriving (Eq)

instance L.LambdaTerm Term where
    redexExists = redexExists
    betaReduce = betaReduce

instance Show Term where
    show (BVar n) = show n
    show (FVar a) = 'f' : (show a)
    show (Lam t) = '\\' : '.' : '(' : ((show t) ++ ")")
    show (App t t') = '(' : ((show t) ++ " " ++ (show t') ++ ")")

betaReduce :: Term -> Term
betaReduce (BVar n) = BVar n
betaReduce (FVar a) = FVar a
betaReduce (Lam t) = Lam $ betaReduce t
betaReduce (App (Lam t) t') = open t' Zero t
betaReduce (App t t') 
    | redexExists t = App (betaReduce t) t'
    | otherwise = App t $ betaReduce t'

redexExists :: Term -> Bool
redexExists (BVar _) = False
redexExists (FVar _) = False
redexExists (Lam t) = redexExists t
redexExists (App (Lam _) _) = True
redexExists (App t t') = (redexExists t) || (redexExists t')

open :: Term -> Atom -> Term -> Term
open t n (BVar n') = if n == n' then t else BVar n'
open _ _ (FVar a) = FVar a
open t n (Lam t') = Lam $ open t (Succ n) t'
open t n (App t' t'') = App (open t n t') (open t n t'')

isLocallyClosed :: Term -> Bool
isLocallyClosed t = isLocallyClosed' Zero t

isLocallyClosed' :: Nat -> Term -> Bool
isLocallyClosed' n (BVar n') = n > n'
isLocallyClosed' _ (FVar _) = True
isLocallyClosed' n (Lam t') = isLocallyClosed' (Succ n) t'
isLocallyClosed' n (App t' t'') = (isLocallyClosed' n t') && (isLocallyClosed' n t'')

-- | Error checking for terms to ensure they are locally closed.  Used for 
-- parsing terms.
locallyClosedCheck :: Term -> Either String Term
locallyClosedCheck t
    | isLocallyClosed t = Right t
    | otherwise = Left "Invalid term, not locally closed.  Check bound variable indices."

