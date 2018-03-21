{-|
Module      : LegacyPure
Description : No longer needed De Bruijn indexed untyped lambda calculus 
                implementation
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}
module LegacyPure(
    Term(..),
    termParser
    ) where

import LegacyPure.Term
import LegacyPure.Parser
