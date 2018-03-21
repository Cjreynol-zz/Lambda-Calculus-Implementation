{-|
Module      : SimpleTypes
Description : Simply tymped lambda calculus
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}
module SimpleTypes(
    Term(..),
    Type(..),
    Context,
    newContext,
    typeCheck,
    termParser
    ) where

import SimpleTypes.Term
import SimpleTypes.Parser
