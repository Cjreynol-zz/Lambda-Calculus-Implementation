{-|
Module      : SimpleTypes.Type
Description : Types for STLC
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}
module SimpleTypes.Type(
      Type(..)
    ) where


-- | Alias used for type variables.
type TVar = Char

-- | Simple types, consisting of Arrow types (T -> T') and type variables.
data Type = Arrow Type Type |
            TypeVar TVar

instance Eq Type where
    (==) (Arrow t1 t2) (Arrow t1' t2') = t1 == t2 && (t1' == t2')
    (==) (TypeVar v) (TypeVar v') = v == v'
    (==) _ _ = False

instance Show Type where
    show (Arrow t t') = '(' : (show t) ++ " -> " ++ (show t') ++ ")"
    show (TypeVar v) = [v]

