{-|
Module      : SimpleTypes.Term
Description : Lambda terms  and type checker for STLC
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}
module SimpleTypes.Term(
    Term,
    typedBetaEq,
    typeCheckAndTerm
    ) where


import LambdaTerm                   (LambdaTerm(..), open)
import Nat                          (Nat(..))
import Context                      (Context, addToContext, atomLookup, 
                                        freshFVar)
import SimpleTypes.Type             (Type(..))
import SimpleTypes.TypingError      (TypingError(..))


-- | Lambda terms with simple types.
type Term = LambdaTerm Type

-- | Beta equality of STLC terms, dependent on a given typing context.  If 
-- the types are valid and equal, then the terms are reduced to pure LC terms 
-- and compared.
typedBetaEq :: Context Type -> Term -> Term -> Bool
typedBetaEq c t1 t2 = tyChF t1 && (tyChF t2) && (t1 == t2)
    where 
        tyChF t = either (\_ -> False) (\_ -> True) (typeCheck t c)

-- | Generates types from annotated STLC terms, or returns an error if the 
-- type annotations or term are incorrect.
typeCheck :: Term -> Context Type -> Either TypingError Type
typeCheck t c = typeCheck' t c

typeCheck' :: Term -> Context Type -> Either TypingError Type
typeCheck' (BVar _) _ = Left $ TyErr "Reached bound variable, term was not locally closed."
typeCheck' (FVar a) c = maybe left right $ atomLookup a c
    where
        left = Left $ TyErr "Variable did not exist in the context."
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
        argMismatch = Left $ TyErr "Argument type does not match argument to arrow type"
        noArrow = Left $ TyErr "Attempting to apply argument to non-arrow type"
        ty1 = typeCheck' t c
        ty2 = typeCheck' t' c

-- | Pairs type checking result with the term, for use after parsing to output 
-- the relevant information.
typeCheckAndTerm :: Term -> Context Type -> Either TypingError (Type,Term)
typeCheckAndTerm t c = typeCheck t c >>= \x -> Right (x,t)

