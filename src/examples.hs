module LamExamples where

import PosInt
import Term
import Reduction


varOne :: Term
varOne = Var One

varTwo :: Term
varTwo = Var (Succ(One))

varThree :: Term
varThree = Var (Succ(Succ(One)))


---------------------------------------
--      SKI Combinators
---------------------------------------
-- S = \xyz.xz(yz)
-- K = \xy.x
-- I = \x.x
---------------------------------------
s :: Term
s = Lam(Lam(Lam(App(App varThree varOne)(App varTwo varOne))))

k :: Term
k = Lam(Lam(varTwo))

i :: Term
i = Lam(varOne)


---------------------------------------
--      Term Examples
---------------------------------------
testTerm1 = (App varOne (Lam varOne))
testTerm2 = (App (Lam (App varOne varOne)) i)
testTerm3 = (App i i)
testTerm4 = (App (App (Lam varOne) (varOne)) (App (Lam varOne) (varOne)))


---------------------------------------
--      Equality Examples
---------------------------------------
testReduceEqual = (betaReduce testTerm1) == testTerm1
testReduceEqual1 = (betaReduce testTerm2) == testTerm3

testReduceNotEqual = (betaReduce testTerm4) /= (normalize testTerm4)
