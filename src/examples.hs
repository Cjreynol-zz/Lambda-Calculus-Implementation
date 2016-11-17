module LamExamples where

import PosInt
import Term


-- S = \xyz.xz(yz)
-- K = \xy.x
-- I = \x.x
varOne :: Term
varOne = Var One

varTwo :: Term
varTwo = Var (Succ(One))

varThree :: Term
varThree = Var (Succ(Succ(One)))

s :: Term
s = Lam(Lam(Lam(App(App varThree varOne)(App varTwo varOne))))

k :: Term
k = Lam(Lam(varTwo))

i :: Term
i = Lam(varOne)

