{-|
Module      : Context
Description : Typing context for typechecking STLC terms
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}
module Context(
    Context,
    addToContext,
    atomLookup,
    emptyContext,
    freshFVar,
    newContext
    ) where


import qualified    Data.Bifunctor  as Bif  (first, second)
import              Data.List               (delete)
import qualified    Data.Map.Strict as Map  (Map, empty, insert, lookup)

import              Nat                     (Atom, allNats)


-- | Represents typing context for STLC terms, used to store variable 
-- references and their associated type for looking up during type-checking.  
-- Also stores a list of "fresh" variables for variable opening.
data Context ty = Ctx (Map.Map Atom ty, [Atom])

first :: ((Map.Map Atom ty) -> (Map.Map Atom ty')) -> Context ty -> Context ty'
first f (Ctx pair) = Ctx $ Bif.first f pair

second :: ([Atom] -> [Atom]) -> Context ty -> Context ty
second f (Ctx pair) = Ctx $ Bif.second f pair

-- | A context with no variables inside
emptyContext :: Context ty
emptyContext = Ctx (Map.empty, allNats)

-- | Adds the given list of atom,type pairs to an empty context.
newContext :: [(Atom, ty)] -> Context ty
newContext l = foldr helper emptyContext l
    where
        helper :: (Atom, ty) -> Context ty -> Context ty
        helper (a,ty) c = addToContext a ty c

-- | Add the atom/type pair to the context, ensuring the atom is also not 
-- available in the list of fresh atoms
addToContext :: Atom -> ty -> Context ty -> Context ty
addToContext a ty c = first (\x -> Map.insert a ty x) $ removeAtomFromFresh a c

removeAtomFromFresh :: Atom -> Context ty -> Context ty
removeAtomFromFresh a c = second (\x -> delete a x) c

-- | Attempt to lookup the type of the atom in the context
atomLookup :: Atom -> Context ty -> Maybe ty
atomLookup a (Ctx (m, _)) = Map.lookup a m

-- | Returns a fresh atom and the context with that atom removed
freshFVar :: Context ty -> (Atom, Context ty)
freshFVar (Ctx (m,(x:xs))) = (x,Ctx (m,xs))
freshFVar _ = error "Out of free variable names, somehow."

