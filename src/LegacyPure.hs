{-|
Module      : LegacyPure
Description : Older implementation of De Bruijn indexed untyped lambda 
                calculus, kept around because it still works
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}
module LegacyPure(
    Term(..),
    termParser
    ) where


import LegacyPure.Parser
import LegacyPure.Term
