module PosInt where 

data PosInt =   Succ PosInt | 
                One
    deriving (Eq, Ord)

instance Show PosInt where
    show n = show (pi2int n)

pi2int :: PosInt -> Int
pi2int One = 1
pi2int (Succ n) = 1 + (pi2int n)

piPred :: PosInt -> PosInt
piPred (Succ n) = n
piPred One = error "Only positive ints"

piAdd :: PosInt -> PosInt -> PosInt
piAdd (One) n2 = Succ n2
piAdd n1 (One) = Succ n1
piAdd (Succ n1) n2 = piAdd n1 (Succ n2)
