{-|
Module      : LambdaTerm
Description : Typeclass for use with command-line interface
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}
module LambdaTerm(
    LambdaTerm(..)
    ) where


-- | Enforces functions needed by main execution loop, provides default 
-- implementations of some for convenience.
class Show a => LambdaTerm a where

    -- | Determine if a beta reducible expression exists within the term.
    redexExists :: a -> Bool

    -- | Reduces a term one step using leftmost reduction to guarantee reaching a 
    -- normal form if it exists.
    betaReduce :: a -> a

    -- | Beta reduces until a normal form is reached.
    normalize :: a -> a
    normalize t
        | redexExists t = normalize $ betaReduce t
        | otherwise = t

    -- | Accumulates each step of the reduction of term t to normal form as a 
    -- String.
    showReductionSteps :: a -> String
    showReductionSteps t = helper . getSteps $ t
        where 
            getSteps t' = case redexExists t' of
                            True -> t' : (getSteps $ betaReduce t')
                            False -> [t']
            helper [] = ""
            helper (x:[]) = show x
            helper (x:xs) = (show x) ++ " ~>\n" ++ (helper xs)
