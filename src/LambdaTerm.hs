{-|
Module      : LambdaTerm
Description : Generic form of typed lambda terms
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}
module LambdaTerm(
    LambdaTerm(..),
    betaReduce,
    opening,
    normalize,
    redexExists,
    showNoTypes,
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
    show (Lam typ t) = '(' : '\\' : ':' : (show typ) ++ "." ++ (show t) ++ ")"
    show (App t1 t2) = '(' : ((show t1) ++ " " ++ (show t2) ++ ")")

showNoTypes :: Show ty => LambdaTerm ty -> String
showNoTypes (BVar n) = show n
showNoTypes (FVar n) = 'f' : (show n)
showNoTypes (Lam _ t) = '(' : '\\' : '.' : (showNoTypes t) ++ ")"
showNoTypes (App t1 t2) = '(' : ((showNoTypes t1) ++ " " ++ (showNoTypes t2) ++ ")")

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
betaReduce (App (Lam _ t) t') = opening t' Zero t
betaReduce (App t1 t2) 
    | redexExists t1 = App (betaReduce t1) t2
    | otherwise = App t1 $ betaReduce t2

-- | Used on lambda abstractions, replaces all instances of the variables 
-- bound to that binder with the given term.  
opening :: LambdaTerm ty -> Atom -> LambdaTerm ty -> LambdaTerm ty
opening t k (BVar n) = if k == n then t else BVar n
opening _ _ (FVar a) = FVar a
opening t k (Lam typ t') = Lam typ $ opening t (Succ k) t'
opening t k (App t1 t2) = App (opening t k t1) (opening t k t2)

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

