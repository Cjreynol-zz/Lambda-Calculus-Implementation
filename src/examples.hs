module LamExamples where

import Nat
import Term
import Reduction

varZero = Var Zero

varOne = Var (Succ(Zero))

varTwo = Var (Succ(Succ(Zero)))


---------------------------------------
--      SKI Combinators
---------------------------------------
-- S = \xyz.xz(yz)
-- K = \xy.x
-- I = \x.x
---------------------------------------
s :: Term
s = Lam(Lam(Lam(App(App varTwo varZero)(App varOne varZero))))

k :: Term
k = Lam(Lam(varOne))

i :: Term
i = Lam(varZero)


---------------------------------------
--      Term Examples
---------------------------------------
testTerm1 = (App varZero (Lam varZero))
testTerm2 = (App (Lam (App varZero varZero)) i)
testTerm3 = (App i i)
testTerm4 = (App (App (Lam varZero) (varZero)) (App (Lam varZero) (varZero)))


---------------------------------------
--      Equality Examples
---------------------------------------
testReduceEqual = (betaReduce testTerm1) == testTerm1
testReduceEqual1 = (betaReduce testTerm2) == testTerm3

testReduceNotEqual = (betaReduce testTerm4) /= (normalize testTerm4)
