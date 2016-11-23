module Nat where 

data Nat =   Succ Nat | 
                Zero
    deriving (Eq, Ord)

instance Show Nat where
    show n = show (nat2int n)

nat2int :: Nat -> Int
nat2int (Zero) = 1
nat2int (Succ n) = 1 + (nat2int n)

natPred :: Nat -> Nat
natPred (Succ n) = n
natPred (Zero) = error "Only positive ints"

natAdd :: Nat -> Nat -> Nat
natAdd (Zero) n2 = Succ n2
natAdd n1 (Zero) = Succ n1
natAdd (Succ n1) n2 = natAdd n1 (Succ n2)
