module PrettyPrinter where

import Term
import Nat


laTeXPreamble :: String
laTeXPreamble = "\\documentclass{article}\n\\usepackage{forest}\n\\usepackage{amssymb}\n\\begin{document}\n"

laTeXConclusion :: String
laTeXConclusion = "\\end{document}"

termToLaTeX :: Term -> String
termToLaTeX t = "\\begin{forest}" ++ (termToLaTeXHelper t) ++ "\\end{forest}\n"

termToLaTeXHelper :: Term -> String
termToLaTeXHelper (Var n) = "[" ++ (show n) ++ "]"
termToLaTeXHelper (Lam t) = "[$\\lambda$ " ++ (termToLaTeXHelper t) ++ "]"
termToLaTeXHelper (App t t') = "[@ " ++ (termToLaTeXHelper t) ++ " " ++ (termToLaTeXHelper t') ++ "]"

termListToLaTeX :: [Term] -> String
termListToLaTeX [] = ""
termListToLaTeX (t:[]) = termToLaTeX t
termListToLaTeX (t:ts) = termToLaTeX t ++ "$\\rightsquigarrow{}$\n" ++ termListToLaTeX ts

termListToStr :: [Term] -> String
termListToStr [] = ""
termListToStr (x:[]) = (show x)
termListToStr (x:xs) = (show x) ++ " ~>\n" ++ (termListToStr xs)
