module LamExamples where

import PosInt
import Term
import Eval


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


---------------------------------------
--      Equality Examples
---------------------------------------
testEvalEqual = (eval testTerm1) == testTerm1
testEvalEqual1 = (eval testTerm2) == testTerm3
