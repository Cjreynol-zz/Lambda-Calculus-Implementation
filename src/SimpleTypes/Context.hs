{-|
Module      : SimpleTypes.Context
Description : Typing context for typechecking STLC terms
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}
module SimpleTypes.Context(
    Context,
    addToContext,
    atomLookup,
    freshFVar,
    newContext
    ) where


import              Data.Bifunctor          (first, second)
import              Data.List               (delete)
import qualified    Data.Map.Strict as Map  (Map, empty, insert, lookup)

import              Nat                     (Atom, allNats)
import              SimpleTypes.Type        (Type)


-- | Represents typing context for STLC terms, used to store variable 
-- references and their associated type for looking up during type-checking.  
-- Also stores a list of "fresh" variables for variable opening.
type Context = (Map.Map Atom Type, [Atom])

emptyContext :: Context
emptyContext = (Map.empty, allNats)

-- | Adds the given list of atom,type pairs to an empty context.
newContext :: [(Atom, Type)] -> Context
newContext l = foldr helper emptyContext l
    where
        helper :: (Atom, Type) -> Context -> Context
        helper (a,ty) c = addToContext a ty c

-- | Add the atom/type pair to the context, ensuring the atom is also not 
-- available in the list of fresh atoms
addToContext :: Atom -> Type -> Context -> Context
addToContext a ty c = first (\x -> Map.insert a ty x) $ removeAtomFromFresh a c

removeAtomFromFresh :: Atom -> Context -> Context
removeAtomFromFresh a c = second (\x -> delete a x) c

-- | Attempt to lookup the type of the atom in the context
atomLookup :: Atom -> Context -> Maybe Type
atomLookup a (m, _) = Map.lookup a m

-- | Returns a fresh atom and the context with that atom removed
freshFVar :: Context -> (Atom, Context)
freshFVar (m,(x:xs)) = (x,(m,xs))
freshFVar _ = error "Out of free variable names, somehow."

