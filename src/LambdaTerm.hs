{-|
Module      : LambdaTerm
Description : Typeclass for use with command-line interface
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}
module LambdaTerm(
    LambdaTerm(..),
    betaReduce,
    open,
    normalize,
    redexExists,
    showReductionSteps
    ) where


import  Nat     (Nat(..), Atom)


-- | Generic form of a lambda term that accepts the type as an argument.
-- Uses locally nameless representation.
--
-- Using the constructors, ill formed terms can be created.  The function 
-- locallyClosedCheck can be used after to ensure that the terms are well 
-- formed.  
--
-- Type checking functions for different instantiations of this datatype can 
-- also be used to ensure the terms are well formed and well typed.
data LambdaTerm ty =    BVar Nat |
                        FVar Atom |
                        Lam ty (LambdaTerm ty)|
                        App (LambdaTerm ty) (LambdaTerm ty)

instance Show ty => Show (LambdaTerm ty) where
    show (BVar n) = show n
    show (FVar n) = 'f' : (show n)
    show (Lam typ t) = '\\' : ':' : (show typ) ++ ".(" ++ (show t) ++ ")"
    show (App t t') = '(' : ((show t) ++ " " ++ (show t') ++ ")")

instance Eq (LambdaTerm ty) where
    (==) t1 t2 = treeEq (normalize t1) (normalize t2)

treeEq :: LambdaTerm ty -> LambdaTerm ty -> Bool
treeEq (BVar n) (BVar n') = if n == n' then True else False
treeEq (FVar a) (FVar a') = if a == a' then True else False
treeEq (Lam _ t) (Lam _ t') = treeEq t t'
treeEq (App t1 t2) (App t1' t2') = treeEq t1 t1' && (treeEq t2 t2')
treeEq _ _ = False

-- | Determine if a beta reducible expression exists within the term.
redexExists :: LambdaTerm ty -> Bool
redexExists (BVar _) = False
redexExists (FVar _) = False
redexExists (Lam _ t) = redexExists t
redexExists (App (Lam _ _) _) = True
redexExists (App t1 t2) = (redexExists t1) || (redexExists t2)

-- | Reduces a term one step using leftmost reduction.  
--
-- Guarantees reaching a normal form, if it exists, in untyped terms.
betaReduce :: LambdaTerm ty -> LambdaTerm ty
betaReduce (BVar n) = BVar n
betaReduce (FVar a) = FVar a
betaReduce (Lam typ t) = Lam typ $ betaReduce t
betaReduce (App (Lam _ t) t') = open t' Zero t
betaReduce (App t1 t2) 
    | redexExists t1 = App (betaReduce t1) t2
    | otherwise = App t1 $ betaReduce t2

-- | Used on lambda abstractions, replaces all instances of the variables 
-- bound to that binder with the given term.  
open :: LambdaTerm ty -> Atom -> LambdaTerm ty -> LambdaTerm ty
open t n (BVar n') = if n == n' then t else BVar n'
open _ _ (FVar a) = FVar a
open t n (Lam typ t') = Lam typ $ open t (Succ n) t'
open t n (App t1 t2) = App (open t n t1) (open t n t2)

-- | Beta reduces until a normal form is reached.
normalize :: LambdaTerm ty -> LambdaTerm ty
normalize t
    | redexExists t = normalize $ betaReduce t
    | otherwise = t

-- | Accumulates each step of the reduction of term t to normal form as a 
-- String.
showReductionSteps :: Show ty => LambdaTerm ty -> String
showReductionSteps t = addArrows . getSteps $ t
    where 
        getSteps t' = case redexExists t' of
                        True -> t' : (getSteps $ betaReduce t')
                        False -> [t']
        addArrows [] = ""
        addArrows (x:[]) = show x
        addArrows (x:xs) = (show x) ++ " ~>\n" ++ (addArrows xs)

