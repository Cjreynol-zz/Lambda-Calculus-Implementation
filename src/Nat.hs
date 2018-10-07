{-|
Module      : Nat
Description : Datatype for natural numbers
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}
module Nat(
    Nat(..),
    Atom,
    allNats,
    ) where 


-- | Datatype representing the Natural numbers as Zero and Succ(essors)
-- of Zero.
-- 
-- For example:
-- 0 = Zero
-- 1 = Succ Zero
-- 2 = Succ (Succ Zero) 
data Nat = Succ Nat 
            | Zero

instance Show Nat where
    show n = show (fromEnum n)

instance Eq Nat where
    (==) (Zero) (Zero) = True
    (==) (Succ _) (Zero) = False
    (==) (Zero) (Succ _) = False
    (==) (Succ n) (Succ m) = (==) n m

instance Ord Nat where
    compare (Zero) (Zero) = EQ
    compare (Succ _) (Zero) = GT
    compare (Zero) (Succ _) = LT
    compare (Succ n) (Succ m) = compare n m

instance Enum Nat where
    toEnum i 
        | i > 0 = Succ $ toEnum (i - 1)
        | otherwise = Zero

    fromEnum (Zero) = 0
    fromEnum (Succ n) = 1 + (fromEnum n)

    pred (Succ n) = n
    pred (Zero) = Zero

    succ (Zero) = Succ Zero
    succ n = Succ n

instance Semigroup Nat where
    (<>) (Zero) n2 = n2
    (<>) n1 (Zero) = n1
    (<>) (Succ n1) n2 = n1 <> (Succ n2)

instance Monoid Nat where
    mempty = Zero

-- | Type for free variable labels, to help distinguish from bound variables
type Atom = Nat

-- | Infinite list of all the natural numbers in increasing order from Zero.
allNats :: [Nat]
allNats = iterate (\x -> Succ x) Zero

