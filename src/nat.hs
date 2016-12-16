module Nat where 

data Nat = Succ Nat | Zero

instance Show Nat where
    show n = show (natToInt n)

instance Eq Nat where
    (==) (Zero) (Zero) = True
    (==) (Succ n) (Zero) = False
    (==) (Zero) (Succ m) = False
    (==) (Succ n) (Succ m) = (==) n m

instance Ord Nat where
    compare (Zero) (Zero) = EQ
    compare (Succ n) (Zero) = GT
    compare (Zero) (Succ m) = LT
    compare (Succ n) (Succ m) = compare n m

natPred :: Nat -> Nat
natPred (Succ n) = n
natPred (Zero) = Zero

natAdd :: Nat -> Nat -> Nat
natAdd (Zero) n2 = Succ n2
natAdd n1 (Zero) = Succ n1
natAdd (Succ n1) n2 = natAdd n1 (Succ n2)

natToInt :: Nat -> Int
natToInt (Zero) = 0
natToInt (Succ n) = 1 + (natToInt n)

intToNat :: Int -> Nat
intToNat i = case (i > 0) of
                True -> Succ (intToNat (i-1))
                False -> Zero
