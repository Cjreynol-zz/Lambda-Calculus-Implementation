module Nat where 

data Nat =   Succ Nat | 
                Zero
    deriving (Eq, Ord)

instance Show Nat where
    show n = show (nat2int n)

nat2int :: Nat -> Int
nat2int (Zero) = 0
nat2int (Succ n) = 1 + (nat2int n)

int2nat :: Int -> Nat
int2nat i = case (i > 0) of
                True -> Succ (int2nat (i-1))
                False -> Zero

natPred :: Nat -> Nat
natPred (Succ n) = n
natPred (Zero) = Zero

natAdd :: Nat -> Nat -> Nat
natAdd (Zero) n2 = Succ n2
natAdd n1 (Zero) = Succ n1
natAdd (Succ n1) n2 = natAdd n1 (Succ n2)
