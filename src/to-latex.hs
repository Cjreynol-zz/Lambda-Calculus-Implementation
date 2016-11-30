module ToLaTeX where

import Term
import Nat


preamble :: String
preamble = "\\documentclass{article}\n\\usepackage{forest}\n\\begin{document}\n"

postamble :: String
postamble = "\\end{document}"

term2LaTeX :: Term -> String
term2LaTeX t = "\\begin{forest}" ++ (term2LaTeXHelper t) ++ "\\end{forest}\n"

term2LaTeXHelper :: Term -> String
term2LaTeXHelper (Var n) = "[" ++ (show n) ++ "]"
term2LaTeXHelper (Lam t) = "[$\\lambda$ " ++ (term2LaTeXHelper t) ++ "]"
term2LaTeXHelper (App t t') = "[@ " ++ (term2LaTeXHelper t) ++ " " ++ (term2LaTeXHelper t') ++ "]"

termList2LaTeX :: [Term] -> String
termList2LaTeX [] = ""
termList2LaTeX (t:ts) = term2LaTeX t ++ termList2LaTeX ts
