# Project Proposal for CS:5850 Programming Language Foundations

My plan is to work on the suggested ”Implementing pure lambda calculus”
project. My implementation would be done in Haskell, encoding the lambda terms
as a Haskell datatype and using De Bruijn indices to avoid variable capture.
The output would be some form of LATEX markup, showing syntax trees or the
terms (or both!). My plan for parsing input is to use the Haskell Parsec
library, which simplifies the creation of simple language parsers.
