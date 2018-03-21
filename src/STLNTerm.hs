module STLNTerm(
    STLNTerm(..),
    SType(..),
    Context,
    newContext,
    typeCheck
    ) where


import              Data.Bifunctor          (first, second)
import              Data.List               (delete)
import qualified    Data.Map.Strict as Map  (Map(..), empty, insert, lookup)

import qualified    LNTerm          as T    (LNTerm(..), Atom)
import              Nat                     (Nat(..), allNats)


type Var = Char

data SType = Arrow SType SType |
            TypeVar Var
            deriving (Eq)

instance Show SType where
    show (Arrow t t') = '(' : (show t) ++ " -> " ++ (show t') ++ ")"
    show (TypeVar v) = show v

data STLNTerm =     BVar Nat |
                    FVar T.Atom |
                    Lam SType STLNTerm |
                    App STLNTerm STLNTerm
                    deriving (Eq)

instance Show STLNTerm where
    show (BVar n) = show n
    show (FVar n) = 'f' : (show n)
    show (Lam ty t) = '\\' : ':' : (show ty) ++ ".(" ++ (show t) ++ ")"
    show (App t t') = '(' : ((show t) ++ " " ++ (show t') ++ ")")

freeVars :: STLNTerm -> [T.Atom]
freeVars (BVar _) = []
freeVars (FVar a) = [a]
freeVars (Lam _ t) = freeVars t
freeVars (App t t') = (freeVars t) ++ (freeVars t')

open :: STLNTerm -> T.Atom -> STLNTerm -> STLNTerm
open t n (BVar n') = if n == n' then t else BVar n'
open _ _ (FVar a) = FVar a
open t n (Lam ty t') = Lam ty $ open t (Succ n) t'
open t n (App t' t'') = App (open t n t') (open t n t'')

type Context = (Map.Map T.Atom SType, [T.Atom])

emptyContext :: Context
emptyContext = (Map.empty, allNats)

newContext :: [(T.Atom, SType)] -> Context
newContext l = foldr helper emptyContext l
    where
        helper :: (T.Atom, SType) -> Context -> Context
        helper (a,ty) c = addToContext a ty $ removeAtomFromFresh a c

removeAtomFromFresh :: T.Atom -> Context -> Context
removeAtomFromFresh a c = second (\x -> delete a x) c

atomLookup :: T.Atom -> Context -> Maybe SType
atomLookup a (m, _) = Map.lookup a m

addToContext :: T.Atom -> SType -> Context -> Context
addToContext a ty c = first (\x -> Map.insert a ty x) c

freshFVar :: Context -> (T.Atom, Context)
freshFVar (m,(x:xs)) = (x,(m,xs))
freshFVar _ = error "Out of free variable names, somehow."

stripType :: STLNTerm -> T.LNTerm
stripType = undefined

isLocallyClosed :: STLNTerm -> Bool
isLocallyClosed = undefined

typeCheck :: STLNTerm -> Context -> Either String SType
typeCheck t c = typeCheck' t c

typeCheck' :: STLNTerm -> Context -> Either String SType
typeCheck' (BVar _) _ = Left "Invalid type, reached bound variable."
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

exampleContext = newContext [(Zero, TypeVar 'T')]
exampleTerm = (App (Lam (TypeVar 'T') (BVar Zero)) (FVar Zero))
exampleBadTerm = (App (Lam (TypeVar 'V') (BVar Zero)) (FVar Zero))
