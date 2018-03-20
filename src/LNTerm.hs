module LNTerm(
    LNTerm(..),
    isLocallyClosed,
    ) where


import qualified    LambdaTerm as L     (LambdaTerm(..))
import              Nat                 (Nat(..))


type Atom = Nat

data LNTerm =   BVar Nat |
                FVar Atom |
                Lam LNTerm |
                App LNTerm LNTerm
                deriving (Eq)

instance L.LambdaTerm LNTerm where
    redexExists = LNTerm.redexExists
    betaReduce = LNTerm.betaReduce

instance Show LNTerm where
    show (BVar n) = show n
    show (FVar n) = 'f' : (show n)
    show (Lam t) = '\\' : '.' : '(' : ((show t) ++ ")")
    show (App t t') = '(' : ((show t) ++ " " ++ (show t') ++ ")")

betaReduce :: LNTerm -> LNTerm
betaReduce (BVar n) = BVar n
betaReduce (FVar n) = FVar n
betaReduce (Lam t) = Lam $ betaReduce t
betaReduce (App (Lam t) t') = open t' Zero t
betaReduce (App t t') 
    | redexExists t = App (betaReduce t) t'
    | otherwise = App t $ betaReduce t'

redexExists :: LNTerm -> Bool
redexExists (BVar _) = False
redexExists (FVar _) = False
redexExists (Lam t) = redexExists t
redexExists (App (Lam _) _) = True
redexExists (App t t') = (redexExists t) || (redexExists t')

open :: LNTerm -> Nat -> LNTerm -> LNTerm
open t n (BVar n') = if n == n' then t else BVar n'
open t n (FVar n') = FVar n'
open t n (Lam t') = Lam $ open t (Succ n) t'
open t n (App t' t'') = App (open t n t') (open t n t'')

isLocallyClosed :: LNTerm -> Bool
isLocallyClosed t = isLocallyClosed' Zero t
    where
        isLocallyClosed' :: Nat -> LNTerm -> Bool
        isLocallyClosed' n (BVar n') = n > n'
        isLocallyClosed' n (FVar _) = True
        isLocallyClosed' n (Lam t) = isLocallyClosed' (Succ n) t
        isLocallyClosed' n (App t t') = (isLocallyClosed' n t) && (isLocallyClosed' n t')

