{-|
Module      : SimpleTypes.Term
Description : Terms(and other types) for STLC
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}
module SimpleTypes.Term(
    Term(..),
    Type(..),
    Context,
    newContext,
    typeCheck
    ) where


import              Data.Bifunctor          (first, second)
import              Data.List               (delete)
import qualified    Data.Map.Strict as Map  (Map, empty, insert, lookup)

import qualified    Pure.Term       as T    (Term(..), Atom)
import              Nat                     (Nat(..), allNats)


-- | Alias used for type variables.
type TVar = Char

-- | Simple types, consisting of Arrow types (T -> T') and type variables.
data Type = Arrow Type Type |
            TypeVar TVar
            deriving (Eq)

instance Show Type where
    show (Arrow t t') = '(' : (show t) ++ " -> " ++ (show t') ++ ")"
    show (TypeVar v) = show v

-- | Terms in STLC, using type annotations on lambda binders to ensure all 
-- properly typed terms can have their full type determined.
data Term = BVar Nat |
            FVar T.Atom |
            Lam Type Term |
            App Term Term
            deriving (Eq)

instance Show Term where
    show (BVar n) = show n
    show (FVar n) = 'f' : (show n)
    show (Lam ty t) = '\\' : ':' : (show ty) ++ ".(" ++ (show t) ++ ")"
    show (App t t') = '(' : ((show t) ++ " " ++ (show t') ++ ")")

open :: Term -> T.Atom -> Term -> Term
open t n (BVar n') = if n == n' then t else BVar n'
open _ _ (FVar a) = FVar a
open t n (Lam ty t') = Lam ty $ open t (Succ n) t'
open t n (App t' t'') = App (open t n t') (open t n t'')

-- | Represents typing context for STLC terms, used to store variable 
-- references and their associated type for looking up during type-checking.  
-- Also stores a list of "fresh" variables for variable opening.
type Context = (Map.Map T.Atom Type, [T.Atom])

emptyContext :: Context
emptyContext = (Map.empty, allNats)

-- | Adds the given list of atom,type pairs to an empty context.
newContext :: [(T.Atom, Type)] -> Context
newContext l = foldr helper emptyContext l
    where
        helper :: (T.Atom, Type) -> Context -> Context
        helper (a,ty) c = addToContext a ty $ removeAtomFromFresh a c

removeAtomFromFresh :: T.Atom -> Context -> Context
removeAtomFromFresh a c = second (\x -> delete a x) c

atomLookup :: T.Atom -> Context -> Maybe Type
atomLookup a (m, _) = Map.lookup a m

-- Should I remove atoms here instead of in newContext?
addToContext :: T.Atom -> Type -> Context -> Context
addToContext a ty c = first (\x -> Map.insert a ty x) c

freshFVar :: Context -> (T.Atom, Context)
freshFVar (m,(x:xs)) = (x,(m,xs))
freshFVar _ = error "Out of free variable names, somehow."

stripType :: Term -> T.Term
stripType (BVar n) = T.BVar n
stripType (FVar a) = T.FVar a
stripType (Lam _ t) = T.Lam $ stripType t
stripType (App t t') = T.App (stripType t) (stripType t')

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

