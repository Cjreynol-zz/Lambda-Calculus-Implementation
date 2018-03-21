{-|
Module      : SimpleTypes.Term
Description : Lambda terms  and type checker for STLC
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}
module SimpleTypes.Term(
    Term(..),
    stripTypes,
    typeCheck
    ) where


import              Nat                         (Nat(..), Atom)
import qualified    Pure.Term           as Pure (Term(..))
import              SimpleTypes.Context         (Context, addToContext, 
                                                    atomLookup, freshFVar)
import              SimpleTypes.Type            (Type(..))


-- | Terms in STLC, using type annotations on lambda binders to ensure all 
-- properly typed terms can have their full type determined.
data Term = BVar Nat |
            FVar Atom |
            Lam Type Term |
            App Term Term
            deriving (Eq)

instance Show Term where
    show (BVar n) = show n
    show (FVar n) = 'f' : (show n)
    show (Lam ty t) = '\\' : ':' : (show ty) ++ ".(" ++ (show t) ++ ")"
    show (App t t') = '(' : ((show t) ++ " " ++ (show t') ++ ")")

open :: Term -> Atom -> Term -> Term
open t n (BVar n') = if n == n' then t else BVar n'
open _ _ (FVar a) = FVar a
open t n (Lam ty t') = Lam ty $ open t (Succ n) t'
open t n (App t' t'') = App (open t n t') (open t n t'')

-- | Generates types from annotated STLC terms, or returns an error if the 
-- type annotations or term are incorrect.
typeCheck :: Term -> Context -> Either String Type
typeCheck t c = typeCheck' t c

typeCheck' :: Term -> Context -> Either String Type
typeCheck' (BVar _) _ = Left "Reached bound variable, term was not locally closed."
typeCheck' (FVar a) c = maybe left right $ atomLookup a c
    where
        left = Left "Variable did not exist in the context."
        right = (\x -> Right x)

typeCheck' (Lam ty t) c = case bodyType of
                            (Right ty') -> Right (Arrow ty ty')
                            _ -> bodyType
    where
        (atom, c') = freshFVar c
        bodyType = typeCheck' (open (FVar atom) Zero t) (addToContext atom ty c')

typeCheck' (App t t') c = 
    case ty2 of
        (Right argTy) -> 
            case ty1 of
                (Right funTy) -> 
                    case funTy of
                        (Arrow a1 a2) -> 
                            if a1 == argTy then Right a2 else argMismatch
                        _ -> noArrow
                _ -> ty1
        _ -> ty2
    where
        argMismatch = Left "Argument type does not match argument to arrow type"
        noArrow = Left "Attempting to apply argument to non-arrow type"
        ty1 = typeCheck' t c
        ty2 = typeCheck' t' c

-- | Convert STLC terms to untyped terms, for reduction
stripTypes :: Term -> Pure.Term
stripTypes (BVar n) = Pure.BVar n
stripTypes (FVar a) = Pure.FVar a
stripTypes (Lam _ t) = Pure.Lam $ stripTypes t
stripTypes (App t t') = Pure.App (stripTypes t) (stripTypes t')

