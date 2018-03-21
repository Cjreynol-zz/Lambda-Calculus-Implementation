{-|
Module      : Nat
Description : Datatype for natural numbers
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}
module Nat(
    Nat(..),
    allNats,
    intToNat,
    natAdd,
    natToInt,
    natPred
    ) where 


-- | Datatype representing the Natural numbers as Zero and Succ(essors)
-- of Zero.
-- 
-- 0 = Zero
-- 1 = Succ Zero
-- 2 = Succ (Succ Zero) 
data Nat = Succ Nat | Zero

instance Show Nat where
    show n = show (natToInt n)

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

-- | Infinite list of all the natural numbers in increasing order from Zero.
allNats :: [Nat]
allNats = iterate (\x -> Succ x) Zero

-- | Nat - 1 operation, with a lower bound of Zero.
natPred :: Nat -> Nat
natPred (Succ n) = n
natPred (Zero) = Zero

-- | Nat + Nat operation.  
-- 
-- Accumulates Succ(essors) from first digit onto the other, so this operation 
-- is linear on the size of the first digit unless the second is Zero.
natAdd :: Nat -> Nat -> Nat
natAdd (Zero) n2 = Succ n2
natAdd n1 (Zero) = Succ n1
natAdd (Succ n1) n2 = natAdd n1 (Succ n2)

-- | Builds an integer from a Nat, linear on the size of the Nat.
natToInt :: Nat -> Int
natToInt (Zero) = 0
natToInt (Succ n) = 1 + (natToInt n)

-- | Builds a Nat from an integer, linear on the size of the Int.
-- 
-- Has a lower bound of Zero.
intToNat :: Int -> Nat
intToNat i = case (i > 0) of
                True -> Succ (intToNat (i-1))
                False -> Zero

